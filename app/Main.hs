{-# LANGUAGE CPP #-}

module Main (main) where

import App (start)

#if wasi_HOST_OS
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSaddle.Wasm.run start
#else
import Language.Javascript.JSaddle.Warp qualified as JSaddle.Warp

main :: IO ()
main = JSaddle.Warp.run 8081 start
#endif
