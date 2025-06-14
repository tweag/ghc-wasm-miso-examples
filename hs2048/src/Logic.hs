{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Logic where

import Data.Maybe
import GameModel
import InputModel
import Miso
import System.Random
import Touch

groupedByTwo :: Eq a => [a] -> [[a]]
groupedByTwo [x] = [[x]]
groupedByTwo [x, y] =
  if x == y
    then [[x, y]]
    else [[x], [y]]
groupedByTwo (x:y:xs) =
  if x == y
    then [x, y] : groupedByTwo xs
    else [x] : groupedByTwo (y : xs)
groupedByTwo [] = []

mergeTiles :: [Tile] -> Tile
mergeTiles [x] = x
mergeTiles [x, y] = Tile s (-1, -1) [x, y]
  where
    s = tileValue x + tileValue y

slideRow :: [Tile] -> ([Tile], Int)
slideRow r =
  ( take gridSize (map mergeTiles grouped ++ replicate gridSize Empty)
  , sum . map tileToInt . concat . filter (\x -> length x > 1) $ grouped)
  where
    grouped = groupedByTwo . filter (/= Empty) $ r

rotatedGrid :: Direction -> Grid -> Grid
rotatedGrid D = rotateGrid
rotatedGrid R = rotateGrid . rotateGrid
rotatedGrid U = rotateGrid . rotateGrid . rotateGrid
rotatedGrid _ = id

slidGrid :: Direction -> Grid -> Grid
slidGrid U = rotateGrid
slidGrid R = rotateGrid . rotateGrid
slidGrid D = rotateGrid . rotateGrid . rotateGrid
slidGrid _ = id

slideGrid :: Direction -> Grid -> (Grid, Int)
slideGrid None grid = (grid, 0)
slideGrid dir grid = (newGrid, scoreGained)
  where
    posGrid = updatePosition grid
    rowsWithScores = map slideRow . (\(Grid h) -> h) . rotatedGrid dir $ posGrid
    slidRotatedGrid = Grid (map fst rowsWithScores)
    scoreGained = sum . map snd $ rowsWithScores
    newGrid = slidGrid dir slidRotatedGrid

slideGameState :: GameState -> GameState
slideGameState state@GameState {..} =
  if newGrid == grid
    then state
    else state
         { grid = newGrid
         , score = newScore
         , bestScore = max bestScore newScore
         , scoreAdd = gotScore
         }
  where
    (newGrid, gotScore) = slideGrid direction grid
    newScore = score + gotScore
    newBest = max bestScore newScore

gameLost :: Grid -> Bool
gameLost g = (g /= emptyGrid) && all (== g) [up, down, left, right]
  where
    up = fst . slideGrid U $ g
    down = fst . slideGrid D $ g
    left = fst . slideGrid L $ g
    right = fst . slideGrid R $ g

gameWon :: Grid -> Bool
gameWon (Grid g) = Number 2048 `elem` concat g

lose :: GameState -> GameState
lose gameState = gameState {gameProgress = GameOver}

win :: GameState -> GameState
win gameState = gameState {gameProgress = Won}

tile2Probability :: Float
tile2Probability = 0.9

newTile :: Float -> Tile
newTile x
  | x < tile2Probability = Number 2
  | otherwise = Number 4

emptyTiles :: Grid -> [(Int, Int)]
emptyTiles =
  map (\(_, i, j) -> (i, j)) .
  filter (\(t, _, _) -> t == Empty) . tilesWithCoordinates

newTileIndex :: Float -> Grid -> Maybe (Int, Int)
newTileIndex x g =
  case emptyTileIndices of
    [] -> Nothing
    _ -> Just (emptyTileIndices !! idx)
  where
    emptyTileIndices = emptyTiles g
    idx = (floor . (* x) . fromIntegral . length $ emptyTileIndices) :: Int

placeRandomTile :: GameState -> GameState
placeRandomTile gameState@GameState {..} =
  if isNothing tileIndex
    then gameState
    else gameState
         { grid = setTile (fromMaybe (0, 0) tileIndex) grid $ newTile float2
         , randomSeed = nSeed
         }
  where
    (float1, stdGen1) = random . mkStdGen $ randomSeed
    (float2, stdGen2) = random stdGen1
    (nSeed, _) = random stdGen2
    tileIndex = newTileIndex float1 grid

newGame :: GameState -> GameState
newGame state@GameState {..} = newGame
  where
    newGame =
      placeRandomTile . placeRandomTile $
      defaultGame {randomSeed = randomSeed, bestScore = bestScore}

stepSlide :: GameState -> GameState
stepSlide state =
  if grid pushedState == grid state
    then state
    else placeRandomTile pushedState
  where
    pushedState = slideGameState state {drawScoreAdd = 0}

step :: GameState -> GameState
step state@GameState {..} =
  if | gameProgress == Won || gameProgress == GameOver -> state
     | gameWon grid, gameProgress /= Continuing -> win state
     | gameLost grid -> lose state
     | direction /= None -> stepSlide state
     | otherwise -> state

updateGameState :: Action -> Effect GameState Action
updateGameState Sync = modify $ \state@GameState {..} -> state {drawScoreAdd = scoreAdd}
updateGameState NewGame = modify newGame >> issue Sync
updateGameState Continue = modify $ \state -> state {gameProgress = Continuing}
updateGameState (GetArrows arr) = do
  modify $ \state -> step $ state {direction = toDirection arr}
  issue Sync
updateGameState (TouchStart (TouchEvent touch)) = do
  modify $ \state -> state {prevTouch = Just touch}
    -- putStrLn "Touch did start"
updateGameState (TouchEnd (TouchEvent touch)) = do
  state <- get
  put state {prevTouch = Nothing}
    -- putStrLn "Touch did end"
  let (GetArrows x) =
          swipe (Touch.client . fromJust . prevTouch $ state) (Touch.client touch)
    -- print x
  issue $ swipe (Touch.client . fromJust . prevTouch $ state) (Touch.client touch)
updateGameState Init = issue NewGame
