-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module TwoZeroFourEight where

import Data.Map
-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)
import System.Random

import GameModel
import InputModel
import Logic
import Rendering
import Touch

-- | Entry point for a miso application
start :: JSM ()
start = do
  stdGen <- getStdGen
  let (seed, _) = random stdGen
  startApp App {model = defaultGame {randomSeed = seed}, ..}
  where
    initialAction = Init -- initial action to be executed on application load
    model = defaultGame -- initial model
    update = updateGameState -- update function
    view = display -- view function
    events = union touchEvents defaultEvents -- default delegated events
    mountPoint = Nothing -- defaults to body
    subs = [arrowsSub GetArrows] -- empty subscription list
    logLevel = Off
