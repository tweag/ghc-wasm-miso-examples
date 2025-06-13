{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering where

import qualified Data.Map as M
import GameModel
import InputModel
import Miso
import Miso.String (MisoString, ms)
import qualified Miso.String as S
import qualified Miso.Style as CSS
import Touch

black :: MisoString
black = "rgb(0,0,0)"

rgb :: Int -> Int -> Int -> MisoString
rgb r g b = S.pack $ "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

rgba :: Int -> Int -> Int -> Float -> MisoString
rgba r g b a =
  S.pack $ "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ show a ++ ")"

tileSize :: Float
tileSize = 106.25

tileMargin :: Float
tileMargin = 15

tileColor :: Tile -> MisoString
tileColor tile =
  case tile of
    Number 2 -> rgb 238 228 218
    Number 4 -> rgb 237 224 200
    Number 8 -> rgb 242 177 121
    Number 16 -> rgb 245 149 99
    Number 32 -> rgb 246 124 95
    Number 64 -> rgb 246 94 59
    Number 128 -> rgb 237 207 114
    Number 256 -> rgb 237 204 97
    Number 512 -> rgb 237 200 80
    Number 1024 -> rgb 237 197 63
    Number 2048 -> rgb 237 194 46
    _ -> rgba 238 228 218 0.35 -- empty tile

tileTextColor :: Tile -> MisoString
tileTextColor tile =
  case tile of
    Number n ->
      if n >= 8
        then rgb 249 246 242
        else rgb 119 110 101
    _ -> black

tileTextSize :: Tile -> Float
tileTextSize tile =
  case tile of
    Number 128 -> 45
    Number 256 -> 45
    Number 512 -> 45
    Number 1024 -> 35
    Number 2048 -> 35
    _ -> 55 -- empty tile

tileTextStyle :: Tile -> [(MisoString, MisoString)]
tileTextStyle tile =
  [ ("typeface", "'Helvetica Neue', Arial, sans-serif")
  , ("height", ms . show . tileTextSize $ tile)
  , ("color", tileTextColor tile)
  ]

gameOverOverlayStyle :: [(MisoString, MisoString)]
gameOverOverlayStyle = tileTextStyle (Number 2)

wonOverlayStyle :: [(MisoString, MisoString)]
wonOverlayStyle = tileTextStyle (Number 16)

gameOverOverlayColor :: MisoString
gameOverOverlayColor = rgba 238 228 218 0.73

wonOverlayColor :: MisoString
wonOverlayColor = rgba 237 194 46 0.5

wonMessage :: MisoString
wonMessage = "You won!"

displayHeading :: GameState -> View Action
displayHeading model@GameState {..} =
  div_
    [class_ "heading"]
    [ h1_ [class_ "title"] [text "2048"]
    , div_
        [class_ "scores-container"]
        [ div_
            [class_ "score-container"]
            ((text . ms . show $ score) : scoreAddDiv)
        , div_ [class_ "best-container"] [text . ms . show $ bestScore]
        ]
    ]
  where
    scoreAddDiv =
      case drawScoreAdd of
        0 -> []
        _ ->
          [ div_
              [class_ "score-addition"]
              [text . ms $ "+" ++ show drawScoreAdd]
          ]

displayIntro :: View Action
displayIntro =
  div_
    [class_ "above-game"]
    [ p_
        [class_ "game-intro"]
        [text "Join the numbers and get to the ", strong_ [] [text "2048 tile"]]
    , a_ [class_ "restart-button", onClick NewGame] [text "New Game"]
    ]

displayMessage :: GameState -> View Action
displayMessage state@GameState {..} =
  div_
    [class_ . S.pack $ "game-message" ++ msgClass]
    [ p_ [] [msg]
    , div_
        [class_ "lower"]
        [ a_
            [class_ "keep-playing-button", onClick Continue]
            [text "Keep going"]
        , a_ [class_ "retry-button", onClick NewGame] [text "Try again"]
        ]
    ]
  where
    (msg, msgClass) =
      case gameProgress of
        GameOver -> ("Game Over!", " game-over")
        Won -> ("Game Won!", " game-won")
        _ -> ("", "")

gridRow :: View Action
gridRow = div_ [class_ "grid-row"] (replicate 4 gridCell)
  where
    gridCell = div_ [class_ "grid-cell"] []

displayContainer :: View Action
displayContainer =
  div_
    [class_ "grid-container", onTouchStart TouchStart, onTouchEnd TouchEnd]
    (replicate 4 gridRow)

previousPos :: Tile -> (Int, Int) -> (Int, Int)
previousPos (Number _) = id
previousPos (Tile n pos _) =
  case pos of
    (-1, -1) -> id
    _ -> const pos
previousPos Empty = id

displayTile :: (Tile, Int, Int) -> [View Action]
displayTile (tile, col, row) =
  merges ++
  [ div_
      [class_ . S.pack . unwords $ ["tile", valueClass, posClass, statusClass]]
      [div_ [class_ "tile-inner"] [text . ms $ val]]
  ]
  where
    val = show . tileValue $ tile
    valueClass = "tile-" ++ val
    (prevCol, prevRow) = previousPos tile (col, row)
    posClass =
      "tile-position-" ++
      show (col + 1) ++
      "-" ++
      show (row + 1) ++ "-" ++ show (prevCol + 1) ++ "-" ++ show (prevRow + 1)
    statusClass =
      case tile of
        (Number _) -> "tile-new"
        (Tile _ pos x) ->
          if | null x -> ""
             | pos == (-1, -1) -> "tile-merged"
             | otherwise -> ""
        _ -> ""
    merges =
      case tile of
        (Number _) -> []
        (Tile _ _ x) -> concatMap (displayTile . (\t -> (t, col, row))) x

displayTileContainer :: Grid -> View Action
displayTileContainer grid =
  div_
    [class_ "tile-container"]
    (concatMap displayTile .
     filter (\(t, _, _) -> t /= Empty) . tilesWithCoordinates $
     grid)

displayGame :: GameState -> View Action
displayGame model =
  div_
    [class_ "game-container"]
    [ displayMessage model
    , displayContainer
    , displayTileContainer . grid $ model
    ]

display :: GameState -> View Action
display model =
  div_
    [class_ "container"]
    [displayHeading model, displayIntro, displayGame model]
  where
    preview =
      div_
        [ CSS.style_
          [("left", "100px"), ("width", "100px"), ("position", "absolute")]
        ]
        [text . S.pack . show $ model]
