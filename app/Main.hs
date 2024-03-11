module Main (main) where

import App (start)
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSaddle.Wasm.run start
