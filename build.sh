#!/bin/bash

pushd "$(dirname "$0")" || exit

DIST_DIR=./dist

cabal build wasm-sim

hs_wasm_path=$(find dist-newstyle -name "*.wasm")

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
    --input "$hs_wasm_path" --output $DIST_DIR/ghc_wasm_jsffi.js

cp $hs_wasm_path $DIST_DIR/bin.wasm

# wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o $DIST_DIR/bin.wasm "$hs_wasm_path"
# wasm-opt ${1+"$@"} $DIST_DIR/bin.wasm -o $DIST_DIR/bin.wasm
# wasm-tools strip -o $DIST_DIR/bin.wasm $DIST_DIR/bin.wasm

cp ./src/* ./dist/
