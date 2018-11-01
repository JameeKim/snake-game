module SnakeGame.Event
    ( eventHandler
    )
where

import qualified Graphics.Gloss.Interface.Pure.Game
                                               as G
import           SnakeGame.World                ( World(..)
                                                , newWorld
                                                )
import           SnakeGame.Snake                ( changeDirection )
import           SnakeGame.Move                 ( Direction(..) )

eventHandler :: G.Event -> World -> World
eventHandler event@G.EventKey{} world = keyEventHandler event world
eventHandler (G.EventResize x)  world = resizeEventHandler x world
eventHandler _                  world = world

keyEventHandler :: G.Event -> World -> World
keyEventHandler (G.EventKey (G.SpecialKey key) G.Down _ _) world =
    if worldIsOver world
        then case key of
            G.KeySpace -> newWorld world
            _          -> world
        else case key of
            G.KeyUp    -> changeSnake U
            G.KeyDown  -> changeSnake D
            G.KeyLeft  -> changeSnake L
            G.KeyRight -> changeSnake R
            _          -> world
  where
    changeSnake direction =
        world { worldSnake = changeDirection (worldSnake world) direction }
keyEventHandler _ world = world

resizeEventHandler :: (Int, Int) -> World -> World
resizeEventHandler x world = world { worldResolution = (px, px) }
    where px = uncurry min x
