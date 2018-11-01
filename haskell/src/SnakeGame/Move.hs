module SnakeGame.Move
    ( Direction(..)
    , oppositeDirection
    )
where

data Direction = U | D | L | R deriving (Show, Eq, Enum, Bounded, Ord)

oppositeDirection :: Direction -> Direction
oppositeDirection U = D
oppositeDirection D = U
oppositeDirection L = R
oppositeDirection R = L
