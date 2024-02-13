module App (start) where

import Control.Monad.IO.Class
import Language.Javascript.JSaddle (JSM)
import SimpleCounter qualified
import Snake qualified
import System.Environment
import TodoMVC qualified
import TwoZeroFourEight qualified

start :: JSM ()
start =
  liftIO (getEnv "EXAMPLE") >>= \case
    "simplecounter" -> SimpleCounter.start
    "snake" -> Snake.start
    "todomvc" -> TodoMVC.start
    "2048" -> TwoZeroFourEight.start
    _ -> fail "unknown example"
