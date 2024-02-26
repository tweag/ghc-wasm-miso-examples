#!/usr/bin/env bash
set -e

if [ $# -eq 0 ]; then
    echo "Building for dev"
    dev_mode=true
else
    echo "Building for prod"
    dev_mode=false
fi

rm -rf dist
mkdir dist
cp ./*.html dist/

wasm32-wasi-cabal build ghc-wasm-miso-examples
wasm32-wasi-cabal list-bin ghc-wasm-miso-examples
hs_wasm_path=$(wasm32-wasi-cabal list-bin ghc-wasm-miso-examples)

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
     --input "$hs_wasm_path" --output ghc_wasm_jsffi.js

if $dev_mode; then
    cp "$hs_wasm_path" dist/bin.wasm
else
    wasm-opt ${1+"$@"} "$hs_wasm_path" -o dist/bin.wasm
    wasm-strip --keep-section=name dist/bin.wasm
fi

function bundle() {
    esbuild index.js --bundle --platform=browser --format=esm --outfile=dist/index.js "$@"
}
if $dev_mode; then
    bundle
else
    bundle --minify
fi

# for TodoMVC (from https://todo-mvc.haskell-miso.org):
cp -r todomvc/ dist/

# for 2048:
mkdir -p dist/2048
sass -q --no-source-map ../hs2048/static/main.scss dist/2048/main.css
