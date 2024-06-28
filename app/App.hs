module App (start) where

import GHC.Wasm.Prim
import Language.Javascript.JSaddle (JSM)
import SimpleCounter qualified
import Snake qualified
import TodoMVC qualified
import TwoZeroFourEight qualified
import XHR qualified

start :: JSString -> JSM ()
start e =
  case fromJSString e of
    "simplecounter" -> SimpleCounter.start
    "snake" -> Snake.start
    "todomvc" -> TodoMVC.start
    "xhr" -> XHR.start
    "2048" -> TwoZeroFourEight.start
    _ -> fail "unknown example"
