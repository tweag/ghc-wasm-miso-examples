# `ghc-wasm-miso-examples`

[![Chat on Matrix](https://matrix.to/img/matrix-badge.svg)](https://matrix.to/#/#haskell-wasm:matrix.terrorjack.com)

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
- [xhr](https://tweag.github.io/ghc-wasm-miso-examples/xhr.html)
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

You can set up the toolchain by either:

- Using
  [`ghc-wasm-meta`](https://gitlab.haskell.org/ghc/ghc-wasm-meta#getting-started-without-nix)
  directly to set up ghc head or ghc 9.10
- Using [`ghcup`](https://www.haskell.org/ghcup/guide/#cross-support)
  to set up ghc 9.10

Then:

```sh
source ~/.ghc-wasm/env
npm install -g sass
cd frontend
./build.sh
```

## Acknowledgements

The examples are vendored and modified from the following projects:

- 2048: based on https://github.com/ptigwe/hs2048
- xhr: based on https://github.com/dmjio/miso/blob/master/examples/xhr/Main.hs
- snake: based on https://github.com/lbonn/miso-snake/blob/master/Main.hs
- todomvc: based on https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs
