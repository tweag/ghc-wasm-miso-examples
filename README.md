# `ghc-wasm-miso-examples`

The GHC wasm backend supports the
[JSFFI](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#javascript-ffi-in-the-wasm-backend)
feature, allowing Haskell wasm apps to interop with JavaScript
seamlessly in the browser. This repo contains a few examples to
demonstrate this ability, all of which based on the
[`miso`](https://haskell-miso.org) frontend framework, using an
unofficial [port](https://github.com/amesgen/miso/tree/remove-th) as
well as an experimental
[`jsaddle-wasm`](https://github.com/amesgen/jsaddle-wasm) library
under the hood.

## Live demo

- [2048](https://tweag.github.io/ghc-wasm-miso-examples/2048.html)
- [snake](https://tweag.github.io/ghc-wasm-miso-examples/snake.html)
- [todomvc](https://tweag.github.io/ghc-wasm-miso-examples/todomvc.html)

## Building

### With nix

Within the `nix develop` shell:

```sh
cd frontend
wasm32-wasi-cabal update
./build.sh
```

If you pass additional arguments to `build.sh`, they will be
redirected to `wasm-opt`, otherwise a dev build without `wasm-opt`
will be performed.

The artifacts will be available in `frontend/dist`.

### Without nix

Follow instructions on
[`ghc-wasm-meta`](https://gitlab.haskell.org/ghc/ghc-wasm-meta#getting-started-without-nix)
to set up the toolchain, then:

```sh
source ~/.ghc-wasm/env
npm install -g sass
cd frontend
./build.sh
```

## Acknowledgements

The examples are vendored and modified from the following projects:

- 2048: based on https://github.com/ptigwe/hs2048
- snake: based on https://github.com/lbonn/miso-snake/blob/master/Main.hs
- todomvc: based on https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs
