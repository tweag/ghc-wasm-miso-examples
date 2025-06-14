{-# LANGUAGE RecordWildCards #-}

module InputModel where

import Miso
import Touch

data Direction
  = U
  | D
  | L
  | R
  | None
  deriving (Show, Eq)

data Action
  = Init
  | NewGame
  | GetArrows Arrows
  | Continue
  | Sync
  | TouchStart TouchEvent
  | TouchEnd TouchEvent

toDirection :: Arrows -> Direction
toDirection arr@Arrows {..} =
  case (arrowX, arrowY) of
    (-1, 0) -> L
    (1, 0) -> R
    (0, -1) -> D
    (0, 1) -> U
    _ -> None

swipeDir xDiff yDiff
  | abs xDiff >= abs yDiff = (signum xDiff, 0)
  | otherwise = (0, signum yDiff)

swipe :: (Int, Int) -> (Int, Int) -> Action
swipe (px, py) (x, y) = GetArrows $ Arrows xDir yDir
  where
    xDiff = x - px
    yDiff = py - y
    (xDir, yDir) = swipeDir xDiff yDiff
