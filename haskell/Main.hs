module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import Data.IORef
import Data.List
import GHC.Float
import System.Random

import GHC.Wasm.Prim

main :: IO ()
main = do
    -- Simulation canvas and context, and results charts canvases
    canvas        <- js_document_getElementById (toJSString "game-canvas")
    context       <- js_canvas_getContext canvas (toJSString "2d")
    resultsCanvas <- js_document_getElementById (toJSString "results-canvas")
    ratioCanvas   <- js_document_getElementById (toJSString "ratio-canvas")

    -- Thin lines
    js_canvas_context_lineWidth context 0.5

    -- Make the chart.js charts
    resultsChart <- js_new_results_Chart resultsCanvas
    ratiosChart  <- js_new_ratios_Chart  ratioCanvas

    -- Listen for clicks on the run button
    runButton <- js_document_getElementById (toJSString "sim-input-button")
    runSimulationsCallback <-
      js_as_callback $
        runSimulations resultsChart ratiosChart canvas context
    js_addEventListener runButton (toJSString "click") runSimulationsCallback

runSimulations ::
     JSVal -- ^ Chart object for results
  -> JSVal -- ^ Chart object for ratios
  -> JSVal -- ^ Simulation canvas
  -> JSVal -- ^ Simulation canvas context
  -> IO ()
runSimulations resultsChart ratiosChart canvas context = do
    -- Get inputs
    numSims  <- getNumberOfSimulationsInput
    maxNumCuts  <- getNumberOfCutsInput
    isRandom <- getIsRandomInput
    isSort   <- getIsSortInput

    -- List of cut numbers, potentially sorted
    sortedCuts <- (if isSort then fmap sort else id) . replicateM numSims $ do
      if isRandom then
        randomNumCuts maxNumCuts
      else
        pure maxNumCuts

    -- Clear the canvas, draw the circles
    clearCanvas canvas context
    circles <- drawCircles canvas context numSims

    -- Set up a mutex for drawing and run the simulations
    drawMut <- newMVar ()
    mapM_
      ( runSimulation
          resultsChart
          ratiosChart
          canvas
          context
          drawMut
      )
      ( zip sortedCuts circles
      )
  where
    randomNumCuts :: Int -> IO Int
    randomNumCuts maxNumCuts = do
      t <- randomRIO (0, 1.0)
      return . roundDouble $ 1 + (int2Double maxNumCuts - 1) * t * t;

runSimulation ::
     JSVal
  -- ^ Results chart object
  -> JSVal
  -- ^ Ratios chart object
  -> JSVal
  -- ^ Simulation canvas
  -> JSVal
  -- ^ Simulation canvas context
  -> MVar ()
  -- ^ Drawing mutex
  -> (Int, (Double, Double, Double))
  -- ^ Number of cuts for this circle, paired with the circle's radius and
  -- center coordinates
  -> IO ThreadId
runSimulation resultsChart ratiosChart canvas context drawMut (numCuts, circle) = do
    let simEnv = toSimEnv drawMut numCuts circle
    simulate simEnv `forkFinally` trackResult simEnv
  where
    toSimEnv :: MVar () -> Int -> (Double, Double, Double) -> SimEnv
    toSimEnv drawMut numCuts (rad, x, y) =
        SimEnv
          { simEnv_canvas   = canvas
          , simEnv_context  = context
          , simEnv_delayRef = undefined
          , simEnv_numLines = numCuts
          , simEnv_radius   = rad
          , simEnv_centerX  = x
          , simEnv_centerY  = y
          , simEnv_drawMut  = drawMut
          }

    trackResult :: SimEnv -> Either SomeException Int -> IO ()
    trackResult SimEnv{..} (Right n) = do
        -- Add simulation results points
        js_Chart_add_data
          resultsChart
          0
          simEnv_numLines
          (int2Double n)

        -- Add lower bound points
        js_Chart_add_data
          resultsChart
          1
          simEnv_numLines
          (int2Double $ lowerBound simEnv_numLines)

        -- Add upper bound points
        js_Chart_add_data
          resultsChart
          2
          simEnv_numLines
          (int2Double $ upperBound simEnv_numLines)

        -- Add ratio points
        js_Chart_add_data
          ratiosChart
          0
          simEnv_numLines
          ((int2Double $ upperBound simEnv_numLines) / (int2Double n))

        -- Update charts
        js_Chart_update resultsChart
        js_Chart_update ratiosChart

-- | Run a simulation in the given environment
simulate :: SimEnv -> IO Int
simulate env@SimEnv{..} = do
    go simEnv_numLines 1 []
  where
    go 0 numPieces _ = return numPieces
    go numLines !numPieces drawnLines = do
        -- Generate a random line
        line@Line{..} <- randomLine

        -- need a mutex here to avoid drawing sporadic lines to other circles
        takeMVar simEnv_drawMut
        js_canvas_context_beginPath simEnv_context
        js_canvas_context_moveTo simEnv_context line_startX line_startY
        js_canvas_context_lineTo simEnv_context line_endX line_endY
        js_canvas_context_stroke simEnv_context
        putMVar simEnv_drawMut ()

        -- Check how many of our existing lines it intersects with
        let
          morePieces =
            foldl' (checkIntersect line) 1 drawnLines
        go (numLines - 1) (numPieces + morePieces) (line : drawnLines)

    randomLine :: IO Line
    randomLine = do
        angle1 <- randomRIO (0, 2 * pi)
        angle2 <- randomRIO (0, 2 * pi)
        let
          startX = simEnv_radius * cos angle1 + simEnv_centerX
          startY = simEnv_radius * sin angle1 + simEnv_centerY
          endX   = simEnv_radius * cos angle2 + simEnv_centerX
          endY   = simEnv_radius * sin angle2 + simEnv_centerY
        return
          Line
            { line_startX = startX
            , line_startY = startY
            , line_endX   = endX
            , line_endY   = endY
            }

    checkIntersect line1 acc line2 =
        let
          (intersectX, intersectY) = intersectionPoint line1 line2
          dx = intersectX - simEnv_centerX
          dy = intersectY - simEnv_centerY
          dh = sqrt (dx * dx + dy * dy)
        in
          if dh < simEnv_radius then acc + 1 else acc

    intersectionPoint :: Line -> Line -> (Double, Double)
    intersectionPoint line1 line2 =
        let
          slope1 = lineSlope line1
          slope2 = lineSlope line2
          c1 = line_startY line1 - slope1 * line_startX line1
          c2 = line_startY line2 - slope2 * line_startX line2

          intersectX = (c2 - c1) / (slope1 - slope2)
          intersectY = c1 + intersectX * slope1
        in
          (intersectX, intersectY)

-- | Draw the given number of circles on the canvas and return their radii and
-- central coordinates
drawCircles ::
     JSVal
  -- ^ Simulation canvas
  -> JSVal
  -- ^ Simulation canvas context
  -> Int
  -- ^ Number of simulations
  -> IO [(Double, Double, Double)]
drawCircles canvas context numSims = do
    mapM (drawCircle numSims) [1 .. numSims]
  where
    drawCircle :: Int -> Int -> IO (Double, Double, Double)
    drawCircle total i = do
        let
          n = sqrt $ int2Double total
          w = js_canvas_width canvas
          radius = w / n / 2
          col = (i - 1) `mod` (double2Int n)
          row = (i - 1) `div` (double2Int n)
          centerX = int2Double col * (radius * 2) + radius
          centerY = int2Double row * (radius * 2) + radius

        -- Circle arc
        js_canvas_context_beginPath context
        js_canvas_context_arc
          context
          centerX
          centerY
          radius
          0
          (2 * pi)

        -- Thick red borders
        js_canvas_context_lineWidth context 3
        js_canvas_context_strokeStyle context (toJSString "red")
        js_canvas_context_stroke context

        -- Reset line style
        js_canvas_context_lineWidth context 0.5
        js_canvas_context_strokeStyle context (toJSString "black")
        return (radius, centerX, centerY)

-- | Clear the entire canvas
clearCanvas :: JSVal -> JSVal -> IO ()
clearCanvas canvas ctx = do
    js_canvas_context_clearRect
      ctx
      (js_canvas_width canvas)
      (js_canvas_height canvas)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Environment of the game
data SimEnv =
      SimEnv
        { simEnv_canvas   :: JSVal
        , simEnv_context  :: JSVal
        , simEnv_delayRef :: IORef Int
        , simEnv_numLines :: !Int
        , simEnv_radius   :: !Double
        , simEnv_centerX  :: !Double
        , simEnv_centerY  :: !Double
        , simEnv_drawMut  :: MVar ()
        }

data Line =
      Line
        { line_startX :: !Double
        , line_startY :: !Double
        , line_endX   :: !Double
        , line_endY   :: !Double
        }

lineSlope :: Line -> Double
lineSlope Line{..} = (line_endY - line_startY) / (line_endX - line_startX)

-------------------------------------------------------------------------------
-- Reference functions
-------------------------------------------------------------------------------

lowerBound :: Int -> Int
lowerBound n = n + 1

upperBound :: Int -> Int
upperBound n = (n * n + n + 2) `div` 2

-------------------------------------------------------------------------------
-- View functions
-------------------------------------------------------------------------------

getNumberOfSimulationsInput :: IO Int
getNumberOfSimulationsInput = do
    numSimsSpan <- js_document_getElementById (toJSString "num-sims-value")
    js_innerText numSimsSpan >>= js_parseInt

getNumberOfCutsInput :: IO Int
getNumberOfCutsInput = do
    numCutsInput <- js_document_getElementById (toJSString "num-cuts-input")
    js_input_value numCutsInput

getIsRandomInput :: IO Bool
getIsRandomInput = do
    isRandomInput <- js_document_getElementById (toJSString "random-input")
    js_input_checked isRandomInput

getIsSortInput :: IO Bool
getIsSortInput = do
    isSortInput <- js_document_getElementById (toJSString "sort-input")
    js_input_checked isSortInput

-------------------------------------------------------------------------------
-- Bindings
-------------------------------------------------------------------------------

-- | The entry point
foreign export javascript "hs_start" main :: IO ()


foreign import javascript unsafe "document.getElementById($1)"
  js_document_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.getContext($2)"
  js_canvas_getContext :: JSVal -> JSString -> IO JSVal

foreign import javascript safe "$1.beginPath()"
  js_canvas_context_beginPath :: JSVal -> IO ()

foreign import javascript safe "$1.arc($2, $3, $4, $5, $6)"
  js_canvas_context_arc ::
       JSVal
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> IO ()

foreign import javascript safe "$1.stroke()"
  js_canvas_context_stroke :: JSVal -> IO ()

foreign import javascript safe "$1.moveTo($2, $3)"
  js_canvas_context_moveTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript safe "$1.lineTo($2, $3)"
  js_canvas_context_lineTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript safe "$1.lineWidth = $2"
  js_canvas_context_lineWidth :: JSVal -> Double -> IO ()

foreign import javascript safe "$1.strokeStyle = $2"
  js_canvas_context_strokeStyle :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.width"
  js_canvas_width :: JSVal -> Double

foreign import javascript unsafe "$1.height"
  js_canvas_height :: JSVal -> Double

foreign import javascript safe "$1.clearRect(0, 0, $2, $3)"
  js_canvas_context_clearRect :: JSVal -> Double -> Double -> IO ()

foreign import javascript safe "$1.addEventListener($2, $3)"
  js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript unsafe "parseInt($1, 10)"
  js_parseInt :: JSVal -> IO Int

foreign import javascript unsafe "$1.value"
  js_input_value :: JSVal -> IO Int

foreign import javascript unsafe "$1.checked"
  js_input_checked :: JSVal -> IO Bool

foreign import javascript unsafe "$1.innerText"
  js_innerText :: JSVal -> IO JSVal

-- Yikes!
foreign import javascript unsafe "new Chart($1,{type:'scatter',data:{datasets:[{type:'scatter',label:'Simulation result',data:[],borderWidth:1},{type:'scatter',label:'Lower bound',data:[]},{type:'scatter',label:'Upper bound',data:[]}]},options:{scales:{x:{beginAtZero:true,title:{display:true,text:'Number of cuts'}},y:{beginAtZero:true,title:{display:true,text:'Number of pieces'}}}}})"
  js_new_results_Chart :: JSVal -> IO JSVal

-- Yikes!
foreign import javascript unsafe "new Chart($1,{type:'scatter',data:{datasets:[{type:'scatter',label:'Upper bound divided by simulation result',data:[],borderWidth:1}]},options:{scales:{x:{beginAtZero:true,title:{display:true,text:'Number of cuts'}},y:{beginAtZero:true,title:{display:true,text:'Quotient'}}}}})"
  js_new_ratios_Chart :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.data.datasets[$2].data.push({ x: $3, y: $4 })"
  js_Chart_add_data :: JSVal -> Int -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.update()"
  js_Chart_update :: JSVal -> IO ()

foreign import javascript "wrapper"
  js_as_callback :: IO () -> IO JSVal
