{-# LANGUAGE CPP #-}

#ifdef wasi_HOST_OS
module MyMain (myMain) where
#else
module Main (main) where
#endif

import App (start)
import Miso qualified

#ifdef wasi_HOST_OS
import GHC.Wasm.Prim
#else
import System.Environment (getArgs)
#endif

#ifdef wasi_HOST_OS
myMain :: JSString -> IO ()
myMain e = Miso.run $ start $ fromJSString e
foreign export javascript "hs_start" myMain :: JSString -> IO ()
#else
main :: IO ()
main = getArgs >>= \case
    [arg] -> Miso.run $ start arg
    _ -> fail "bad args: specify an example, e.g. 2048"
#endif
