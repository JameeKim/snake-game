module SnakeGame.Main
    ( main
    )
where

import qualified System.Random                 as R
import qualified Graphics.Gloss                as G
import           SnakeGame.Render               ( draw )
import           SnakeGame.World                ( World(..)
                                                , ups
                                                , newWorld
                                                , update
                                                )
import           SnakeGame.Event                ( eventHandler )

main :: IO ()
main = do
    seed <- R.randomIO
    let world = newWorld seed
    G.play (G.InWindow "testing" (worldResolution world) (0, 0))
           (G.greyN 0.5)
           ups
           world
           draw
           eventHandler
           update
