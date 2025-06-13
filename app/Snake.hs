{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- slightly adapted from https://github.com/lbonn/miso-snake/blob/master/Main.hs

module Snake (start) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Set           as Set
import           Language.Javascript.JSaddle (JSM, askJSM, runJSM)
import           System.Random

import           Miso
import           Miso.String        (MisoString, ms)
import qualified Miso.Style         as CSS
import           Miso.Svg           hiding (height_, id_, style_, width_)

-- | miso-snake: heavily inspired by elm-snake
-- (https://github.com/theburningmonk/elm-snake)

segmentDim, cherryRadius, width, height :: Double
segmentDim = 15
cherryRadius = 7.5
(width, height) = (600, 600)

-- | Utility for periodic tick subscriptions
every :: Int -> (Double -> action) -> Sub action
every n f sink = void . forkJSM . forever $ do
  liftIO $ threadDelay n
  sink =<< f <$> now

start :: JSM ()
start = startComponent Component {..}
  where
    initialAction = Just NoOp
    mountPoint = Nothing
    model  = NotStarted
    update = \a -> get >>= updateModel a
    view   = viewModel
    events = defaultEvents
    subs   = [ directionSub ([38,87],[40,83],[37,65],[39,68]) ArrowPress -- arrows + WASD
             , keyboardSub KeyboardPress
             , every 50000 Tick -- 50 ms
             ]
    logLevel = Off
    styles = []

-- | Model
data Direction
  = U
  | D
  | L
  | R
  deriving (Show, Eq)

type Position = (Double, Double)

pos :: Double -> Double -> Position
pos = (,)

data Snake = Snake
  { shead :: !Position
  , stail :: ![Position]
  , direction :: !Direction
  }
  deriving (Show, Eq)

type Cherry = Maybe Position

type Score = Int

data Model
  = NotStarted
  | Started
    { snake :: !Snake
    , cherry :: !Cherry
    , score :: !Score
    }
  deriving (Show, Eq)

-- | Msg that can trigger updates to Model
data Msg
  = Tick !Double
  | ArrowPress !Arrows
  | KeyboardPress !(Set.Set Int)
  | Spawn !Double !Position
  | NoOp

-- | Initial Snake
initSnake :: Snake
initSnake = Snake { shead = h, stail = t, direction = R }
  where
    h = (height/2, width/2)
    t = fmap (\n -> pos (-n*segmentDim) 0) [1..8]

-- | Render a model
rootBase :: [View a] -> View a
rootBase content = div_ [] [ svg_ [ height_ $ px height
                                  , width_ $ px width
                                  ] [ g_  [] (bg : content) ]
                           ]
  where
    bg = rect_ [ width_ (px width), height_ (px height) ] []


textStyle :: Attribute a
textStyle = CSS.style_ [ ("fill", "green")
                                , ("stroke", "green")
                                , ("text-anchor", "middle")
                                ]

px :: Show a => a -> MisoString
px e = ms $ show e ++ "px"

viewModel :: Model -> View Msg
viewModel NotStarted = rootBase [ text_ [ x_ $ px (width / 2)
                                        , y_ $ px (height / 2)
                                        , textStyle
                                        ] [ text "press SPACE to start" ]
                                ]
viewModel Started{..} =
  rootBase $ scoreLbl : maybe [] (\c -> [cherrySvg c]) cherry ++ snakeSvg snake
  where
    scoreLbl = text_ [ x_ $ px (10 :: Int)
                     , y_ $ px (height - 10)
                     , textStyle
                     ] [ text $ ms $ show score ]
    cherrySvg (x, y) = ellipse_ [ cx_ $ px x
                                , cy_ $ px y
                                , rx_ $ px cherryRadius
                                , ry_ $ px cherryRadius
                                , CSS.style_ [ ("fill", "red")
                                                      , ("stroke", "black")
                                                      , ("stroke-width", "2")
                                                      ]
                                ] []
    snakeSvg Snake {..} = snakeLimb "white" shead : map (snakeLimb "yellow") stail
    snakeLimb color (x, y) = rect_ [ width_ $ px segmentDim
                                   , height_ $ px segmentDim
                                   , x_ $ px x
                                   , y_ $ px y
                                   , CSS.style_ $ [ ("fill", color)
                                                         , ("stroke", "black")
                                                         , ("stroke-width", "2")
                                                         ]
                                   ] []

-- | Updates model, optionally introduces side effects
updateModel :: Msg -> Model -> Effect Model Msg
updateModel msg NotStarted =
  case msg of
       KeyboardPress keys | Set.member 32 keys -> noEff $ Started initSnake Nothing 0
       _                                       -> noEff NotStarted
updateModel (ArrowPress arrs) model@Started{..} =
  let newDir = getNewDirection arrs (direction snake)
      newSnake = snake { direction = newDir } in
  noEff $ model { snake = newSnake }
updateModel (Spawn chance (randX, randY)) model@Started{}
  | chance <= 0.1 =
     let newCherry = spawnCherry randX randY in
     noEff model { cherry = newCherry }
  | otherwise =
     noEff model
updateModel (Tick _) model@Started{..} =
  let newHead = getNewSegment (shead snake) (direction snake)
      ateCherry = maybe False (isOverlap newHead) cherry
      newTail =
        if ateCherry then shead snake : stail snake
                     else shead snake : init (stail snake) -- partial!
      newSnake = snake { shead = newHead, stail = newTail }
      (newCherry, newScore) =
        if ateCherry then (Nothing, score + 1)
                     else (cherry, score)
      newModel = model { snake = newSnake, cherry = newCherry, score = newScore }
      gameOver = isGameOver newHead newTail
      in
  if | gameOver          -> noEff NotStarted
     | cherry == Nothing -> newModel <# do
                                        [chance, xPos, yPos] <- replicateM 3 $ randomRIO (0, 1)
                                        return $ Spawn chance (xPos, yPos)
     | otherwise         -> noEff newModel
updateModel _ model = noEff model

getNewDirection :: Arrows -> Direction -> Direction
getNewDirection (Arrows arrX arrY) dir
  | dir == U || dir == D =
    case arrX of
      -1 -> L
      1 -> R
      _ -> dir
  | otherwise =
    case arrY of
      -1 -> U
      1 -> D
      _ -> dir

getNewSegment :: Position -> Direction -> Position
getNewSegment (x, y) direction =
  case direction of
    U    -> pos x (y+segmentDim)
    D    -> pos x (y-segmentDim)
    L    -> pos (x-segmentDim) y
    R    -> pos (x+segmentDim) y

isGameOver :: Position -> [Position] -> Bool
isGameOver newHead@(x,y) newTail =
  elem newHead newTail   -- eat itself
  || x > width - segmentDim    -- hit right
  || y > height - segmentDim   -- hit bottom
  || x < 0                     -- hit top
  || y < 0                     -- hit left

spawnCherry :: Double -> Double -> Cherry
spawnCherry randW randH =
  let x = randW * (width - 2*cherryRadius) + cherryRadius
      y = randH * (height - 2*cherryRadius) + cherryRadius
  in Just $ pos x y

isOverlap :: Position -> Position -> Bool
isOverlap (snakeX, snakeY) (cherryX, cherryY) =
  let (xd, yd) = ( cherryX - snakeX - (segmentDim /2)
                 , cherryY - snakeY - (segmentDim / 2)
                 )
      distance = sqrt(xd * xd + yd * yd)
  in distance <= (cherryRadius * 2)

forkJSM :: JSM () -> JSM ThreadId
forkJSM a = do
  ctx <- askJSM
  liftIO (forkIO (runJSM a ctx))
