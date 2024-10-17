{-# LANGUAGE CPP #-}

module App (start) where

#ifdef wasi_HOST_OS
import GHC.Wasm.Prim
import Language.Javascript.JSaddle (JSM)
#else
import Language.Javascript.JSaddle
#endif

import SimpleCounter qualified
import Snake qualified
import TodoMVC qualified
import TwoZeroFourEight qualified
import XHR qualified

start :: JSString -> JSM ()
start e =
  case fromJSString e :: String of
    "simplecounter" -> SimpleCounter.start
    "snake" -> Snake.start
    "todomvc" -> TodoMVC.start
    "xhr" -> XHR.start
    "2048" -> TwoZeroFourEight.start
    _ -> fail "unknown example"
