module SnakeGame.Cell
    ( Cell
    , maxCellCoord
    , coordNumToFloat
    )
where

type Cell = (Int, Int)

maxCellCoord :: Int -> Int
maxCellCoord totalCell = totalCell `quot` 2

coordNumToFloat :: Int -> Int -> Int -> Float
coordNumToFloat 0 _ _ = 0.0
coordNumToFloat num size pix =
    fromIntegral num * fromIntegral pix / fromIntegral size
