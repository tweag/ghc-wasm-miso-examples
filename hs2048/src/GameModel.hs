module GameModel where

import Data.List
import InputModel
import Touch

data Tile
  = Number Int
  | Tile Int
         (Int, Int)
         [Tile]
  | Empty

instance Show Tile where
  show (Number n) = show n
  show (Tile n (x, y) _) = show n
  show Empty = "-"

instance Eq Tile where
  (==) (Number x) (Number y) = x == y
  (==) (Tile x _ _) (Tile y _ _) = x == y
  (==) (Number x) (Tile y _ _) = x == y
  (==) (Tile x _ _) (Number y) = x == y
  (==) Empty Empty = True
  (==) _ _ = False

newtype Grid =
  Grid [[Tile]]
  deriving (Eq)

instance Show Grid where
  show (Grid x) = unlines . map show $ x

data Progress
  = InProgress
  | Continuing
  | GameOver
  | Won
  deriving (Show, Eq)

data GameState = GameState
  { grid :: Grid
  , score :: Int
  , scoreAdd :: Int
  , drawScoreAdd :: Int
  , bestScore :: Int
  , gameProgress :: Progress
  , direction :: Direction
  , randomSeed :: Int
  , prevTouch :: Maybe Touch
  } deriving (Show, Eq)

gridSize :: Int
gridSize = 4

tileValue :: Tile -> Int
tileValue (Number n) = n
tileValue (Tile n _ _) = n

readTile :: (Int, Int) -> Grid -> Tile
readTile (i, j) (Grid g) = (g !! j) !! i

setTile :: (Int, Int) -> Grid -> Tile -> Grid
setTile (i, j) (Grid g) t = Grid $ take j g ++ [nr] ++ drop (j + 1) g
  where
    r = g !! j
    nr = take i r ++ [t] ++ drop (i + 1) r

tileToInt :: Tile -> Int
tileToInt (Number n) = n
tileToInt (Tile n _ _) = n
tileToInt Empty = 0

intToTile :: Int -> Tile
intToTile 0 = Empty
intToTile n = Number n

updateTilePosition :: Tile -> (Int, Int) -> Tile
updateTilePosition (Number n) pos = Tile n pos []
updateTilePosition (Tile n _ t) pos = Tile n pos t
updateTilePosition Empty _ = Empty

removeMerges :: Tile -> Tile
removeMerges (Tile n pos _) = Tile n pos []
removeMerges x = x

updatePosition :: Grid -> Grid
updatePosition (Grid g) =
  Grid $
  map (map (uncurry updateTilePosition)) .
  zipWith (\j r -> map (\(t, i) -> (t, (i, j))) r) [0 .. (gridSize - 1)] .
  map ((\r -> zip r [0 .. (gridSize - 1)]) . map removeMerges) $
  g

tilesWithCoordinates :: Grid -> [(Tile, Int, Int)]
tilesWithCoordinates (Grid g) =
  concat .
  zipWith (\j r -> map (\(t, i) -> (t, i, j)) r) [0 .. (gridSize - 1)] .
  map (\r -> zip r [0 .. (gridSize - 1)]) $
  g

rotateGrid :: Grid -> Grid
rotateGrid (Grid g) = Grid (map reverse . transpose $ g)

emptyGrid :: Grid
emptyGrid = Grid $ replicate gridSize . replicate gridSize $ Empty

defaultGame :: GameState
defaultGame =
  GameState
  { grid = emptyGrid
  , score = 0
  , scoreAdd = 0
  , drawScoreAdd = 0
  , bestScore = 0
  , gameProgress = InProgress
  , direction = None
  , randomSeed = 0
  , prevTouch = Nothing
  }
