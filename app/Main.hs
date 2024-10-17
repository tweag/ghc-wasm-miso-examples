{-# LANGUAGE CPP #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import App (start)
import GHC.Wasm.Prim
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main e = JSaddle.Wasm.run $ start e

#else

module Main (main) where

import App (start)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import Network.Wai.Handler.Warp
import Network.WebSockets
import System.Environment

main :: IO ()
main = getArgs >>= \case
    [arg] -> runSettings (setPort 8000 defaultSettings)
        =<< jsaddleOr defaultConnectionOptions (start $ toJSString arg)
            jsaddleApp
    _ -> fail "bad args: specify an example, e.g. 2048"

#endif
