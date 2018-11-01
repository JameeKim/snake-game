module SnakeGame.Render
    ( draw
    )
where

import qualified Graphics.Gloss.Interface.Pure.Game
                                               as G
import           SnakeGame.World                ( World(..)
                                                , worldBorder
                                                , coordNumToFloat
                                                , windowSize
                                                )
import           SnakeGame.Snake                ( Snake(..) )

color :: String -> G.Color
color "border"    = G.black
color "snakeTail" = G.dark G.green
color "snakeHead" = G.green
color "apple"     = G.red
color "text"      = G.black
color "gameOver"  = G.withAlpha 0.2 G.red
color _           = error "Wrong type for pre-defined colors"

draw :: World -> G.Picture
draw world =
    G.pictures $ [drawBorder, drawApple, drawSnake, drawGameOver] <*> [world]

drawGameOver :: World -> G.Picture
drawGameOver world
    | not . worldIsOver $ world
    = G.blank
    | otherwise
    = let
          size  = fromIntegral . windowSize $ world
          score = (length . snakeBody . worldSnake) world - 3
      in
          G.pictures
              [ G.color (color "gameOver") $ G.rectangleSolid size size
              , G.color (color "text")
              $  G.translate (-200) 50
              $  G.scale 0.4 0.4
              $  G.text
              $  "Your score: "
              ++ show score
              , G.color (color "text")
              $ G.translate (-180) (-100)
              $ G.scale 0.2 0.2
              $ G.text "Press space bar to start"
              ]

drawSnake :: World -> G.Picture
drawSnake world =
    G.pictures
        $ (\(h : r) ->
              G.color (color "snakeHead") h
                  : map (G.color (color "snakeTail")) r
          )
        $ map (`drawBox` world)
        $ snakeBody
        . worldSnake
        $ world

drawApple :: World -> G.Picture
drawApple world = G.color (color "apple") $ drawBox (worldApple world) world

drawBorder :: World -> G.Picture
drawBorder world =
    G.pictures
        $ map (G.color (color "border") . (`drawBox` world))
        $ worldBorder world

drawBox :: (Int, Int) -> World -> G.Picture
drawBox (x, y) world = G.translate x' y' $ G.rectangleSolid boxSize boxSize
  where
    boxSize  = coordNumToFloat 1 world
    [x', y'] = map ((boxSize *) . fromIntegral) [x, y]
