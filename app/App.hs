{-# LANGUAGE CPP #-}
module App (start) where

import Language.Javascript.JSaddle (JSM)
import SimpleCounter qualified
import Snake qualified
import TodoMVC qualified
import TwoZeroFourEight qualified
#ifdef wasi_HOST_OS
import XHR qualified
#endif

start :: String -> JSM ()
start e =
  case e of
    "simplecounter" -> SimpleCounter.start
    "snake" -> Snake.start
    "todomvc" -> TodoMVC.start
#ifdef wasi_HOST_OS
    "xhr" -> XHR.start
#endif
    "2048" -> TwoZeroFourEight.start
    _ -> fail "unknown example"
