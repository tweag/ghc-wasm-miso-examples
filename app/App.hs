{-# LANGUAGE CPP #-}

module App (start, App(..)) where

#ifdef wasi_HOST_OS
import GHC.Wasm.Prim
import Language.Javascript.JSaddle (JSM)
#else
import Language.Javascript.JSaddle
#endif

import Data.Text.Lazy (Text)
import SimpleCounter qualified
import Snake qualified
import TodoMVC qualified
import TwoZeroFourEight qualified
import XHR qualified

data App = App
  { name :: Text
  , stylesheets :: [Text]
  , app :: JSM ()
  }

start :: JSString -> App
start e =
  case fromJSString e :: String of
    "simplecounter" ->
      App
        { name = "SimpleCounter"
        , stylesheets = []
        , app = SimpleCounter.start
        }
    "snake" ->
      App
        { name = "Snake"
        , stylesheets = []
        , app = Snake.start
        }
    "todomvc" ->
      App
        { name = "TodoMVC"
        , stylesheets = ["todomvc/base.css", "todomvc/index.css"]
        , app = TodoMVC.start
        }
    "xhr" ->
      App
        { name = "XHR"
        , stylesheets = []
        , app = XHR.start
        }
    "2048" ->
      App
        { name = "TwoZeroFourEight"
        , stylesheets = ["2048/main.css"]
        , app = TwoZeroFourEight.start
        }
    _ -> error "unknown example"
