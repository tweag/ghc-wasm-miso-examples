{-# LANGUAGE OverloadedStrings #-}

module SimpleCounter (start) where

import Control.Monad.State.Strict
import Language.Javascript.JSaddle (JSM)
import Miso
import Miso.String (ms)

start :: JSM ()
start = startComponent Component {..}
  where
    initialAction = Just 0
    model = 0 :: Int
    update n = do
      modify' (+ n)
    view n =
      div_
        []
        [ button_ [onClick -1] [text "-"],
          text $ ms $ show n,
          button_ [onClick 1] [text "+"],
          br_ [],
          a_ [onClickPreventDefault 13, href_ "/bad"] [text "add"]
        ]

    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off
    styles = []

-- https://github.com/dmjio/miso/issues/631
-- but it seems to work fine!
onClickPreventDefault :: a -> Attribute a
onClickPreventDefault a =
  onWithOptions
    (defaultOptions {preventDefault = True})
    "click"
    emptyDecoder
    (\() _ -> a)
