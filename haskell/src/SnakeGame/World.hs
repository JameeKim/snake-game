{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SnakeGame.World
    ( World(..)
    , ups
    , initialWorld
    , newWorld
    , update
    , worldBorder
    , putApple
    , cellIsApple
    , cellIsSnake
    , cellIsBorder
    , maxCoordNum
    , coordNumToFloat
    , windowSize
    )
where

import qualified System.Random                 as R
import qualified SnakeGame.Cell                as C
import qualified SnakeGame.Snake               as S
import           SnakeGame.Cell                 ( Cell )
import           SnakeGame.Snake                ( Snake(..) )
import           SnakeGame.Move                 ( Direction(..) )

data World = NewWorld
    { worldIsOver :: Bool
    , worldResolution :: (Int, Int)
    , worldSize :: Int
    , worldSnake :: Snake
    , worldApple :: Cell
    , worldRandom :: R.StdGen
    } deriving (Show)

size :: Int
size = 21

resolution :: Int
resolution = 1024

ups :: Int
ups = 10

initialWorld :: Int -> World
initialWorld seed = NewWorld
    { worldIsOver     = True
    , worldResolution = (resolution, resolution)
    , worldSize       = size
    , worldSnake      = newSnake maxC
    , worldApple      = newApple maxC
    , worldRandom     = R.mkStdGen seed
    }
    where maxC = C.maxCellCoord size

newWorld :: World -> World
newWorld world = putApple NewWorld
    { worldIsOver     = False
    , worldResolution = worldResolution world
    , worldSize       = wSize
    , worldSnake      = newSnake maxC
    , worldApple      = (0, 0)
    , worldRandom     = r1
    }
  where
    (_ :: Bool, r1) = R.random $ worldRandom world
    wSize           = worldSize world
    maxC            = C.maxCellCoord wSize

update :: Float -> World -> World
update _ world
    | worldIsOver world = world
    | hitItself = world { worldIsOver = True }
    | cellIsBorder world nextHead = world { worldIsOver = True }
    | cellIsApple world nextHead = putApple world
        { worldSnake = S.addHead nextHead snake
        }
    | otherwise = world { worldSnake = S.moveSnake snake }
  where
    nextHead  = S.nextHead snake
    hitItself = S.willHitSelf snake
    snake     = worldSnake world

worldBorder :: World -> [Cell]
worldBorder world = concat [top, bottom, left, right]
  where
    top      = map (, maxCoord) [-maxCoord .. maxCoord - 1]
    bottom   = map (, -maxCoord) [-maxCoord + 1 .. maxCoord]
    left     = map (-maxCoord, ) [-maxCoord .. maxCoord - 1]
    right    = map (maxCoord, ) [-maxCoord + 1 .. maxCoord]
    maxCoord = maxCoordNum world

putApple :: World -> World
putApple world = if cellIsSnake world (x, y)
    then putApple world { worldRandom = gen2 }
    else world { worldApple = (x, y) }
  where
    (x, gen1) = R.randomR (-m, m) $ worldRandom world
    (y, gen2) = R.randomR (-m, m) gen1
    m         = maxCoordNum world - 1

newSnake :: Int -> Snake
newSnake maxC = Snake
    [(-maxC + 3, maxC - 1), (-maxC + 2, maxC - 1), (-maxC + 1, maxC - 1)]
    R

newApple :: Int -> Cell
newApple maxC = (-maxC + 1, maxC - 1)

cellIsApple :: World -> Cell -> Bool
cellIsApple world pt = pt == worldApple world

cellIsSnake :: World -> Cell -> Bool
cellIsSnake world pt = pt `elem` (snakeBody . worldSnake) world

cellIsBorder :: World -> Cell -> Bool
cellIsBorder world pt = pt `elem` worldBorder world

maxCoordNum :: World -> Int
maxCoordNum world = C.maxCellCoord $ worldSize world

coordNumToFloat :: Int -> World -> Float
coordNumToFloat 0 _ = 0.0
coordNumToFloat num world =
    C.coordNumToFloat num (worldSize world) (windowSize world)

windowSize :: World -> Int
windowSize world = uncurry min $ worldResolution world
