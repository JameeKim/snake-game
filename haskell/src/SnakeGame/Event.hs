module SnakeGame.Event
    ( eventHandler
    )
where

import qualified Graphics.Gloss.Interface.Pure.Game
                                               as G
import           SnakeGame.World                ( World(..)
                                                , newWorldVoid
                                                )
import           SnakeGame.Snake                ( changeDirection )
import           SnakeGame.Move                 ( Direction(..) )

eventHandler :: G.Event -> World -> World
eventHandler (G.EventKey key keyState _ _) world =
    keyEventHandler key keyState world
eventHandler _ world = world

keyEventHandler :: G.Key -> G.KeyState -> World -> World
keyEventHandler (G.SpecialKey key) G.Down world@NewWorld { worldSnake = snake }
    = if worldIsOver world
        then case key of
            G.KeySpace -> newWorldVoid world
            _          -> world
        else case key of
            G.KeyUp    -> changeSnake U
            G.KeyDown  -> changeSnake D
            G.KeyLeft  -> changeSnake L
            G.KeyRight -> changeSnake R
            _          -> world
  where
    changeSnake direction =
        world { worldSnake = changeDirection snake direction }
keyEventHandler _ _ world = world
