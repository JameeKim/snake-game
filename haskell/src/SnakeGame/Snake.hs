module SnakeGame.Snake
    ( Snake(..)
    , Cell
    , moveSnake
    , changeDirection
    , addHead
    , willHitSelf
    , nextHead
    )
where

import           SnakeGame.Cell                 ( Cell )
import           SnakeGame.Move                 ( Direction(..)
                                                , oppositeDirection
                                                )

data Snake = Snake { snakeBody :: [Cell], snakeDirection :: Direction } deriving (Show)

moveSnake :: Snake -> Snake
moveSnake snake@(Snake body _) =
    snake { snakeBody = nextHead snake : nextTail snake }

changeDirection :: Snake -> Direction -> Snake
changeDirection snake@(Snake _ oldD) newD
    | newD == oldD                   = snake
    | newD == oppositeDirection oldD = snake
    | otherwise                      = snake { snakeDirection = newD }

addHead :: Cell -> Snake -> Snake
addHead newHead snake@(Snake body _) = snake { snakeBody = newHead : body }

willHitSelf :: Snake -> Bool
willHitSelf snake@(Snake body _) = nextHead snake `elem` nextTail snake

nextHead :: Snake -> Cell
nextHead (Snake ((x, y) : _) U) = (x, y + 1)
nextHead (Snake ((x, y) : _) D) = (x, y - 1)
nextHead (Snake ((x, y) : _) L) = (x - 1, y)
nextHead (Snake ((x, y) : _) R) = (x + 1, y)

nextTail :: Snake -> [Cell]
nextTail (Snake body _) = take (length body - 1) body
