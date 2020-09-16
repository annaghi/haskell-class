module RWH.P070
    ( sortByLength
    , intersperse
    , Tree(..)
    , height
    , count
    , Point
    , Direction(..)
    , direction
    , oppositeDirection
    , turns
    , reflection1
    ) where

import Data.List (sortBy)
import Data.Function (on)
import Prelude hiding (Left, Right)


sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (compare `on` length)


intersperse :: forall a. a -> [[a]] -> [a]
intersperse separator = go
    where
        go :: [[a]] -> [a]
        go []     = []
        go [x]    = x
        go (x:ys) = x ++ separator : (go ys)


data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show)


height :: (Num b, Ord b) => Tree a -> b
height Empty               = (-1)
height (Node _ left right) = 1 + max (height left) (height right)


count :: Num b => Tree a -> b
count Empty               = 0
count (Node _ left right) = 1 + count left + count right


-- type Point a = Num a => (a, a)
type Point a = (a, a)


data Direction
    = Left
    | Right
    | Straight
    deriving (Show, Eq)


direction :: forall a. (Num a, Ord a) => Point a -> Point a -> Point a -> Direction
direction (a1, a2) (b1, b2) (c1, c2) =
    if determinant > 0 then
        Left
    else if determinant < 0 then
        Right
    else
        Straight
    where
        determinant :: a
        determinant = ((b1 - a1) * (c2 - a2)) - ((b2 - a2) * (c1 - a1))


oppositeDirection :: Direction -> Direction
oppositeDirection direction_ =
    case direction_ of
        Left ->
            Right

        Right ->
            Left

        Straight ->
            Straight


turns :: (Num a, Ord a) => [Point a] -> [Direction]
turns []           = []
turns [_]          = []
turns [_,_]        = []
turns (a:b:c:rest) = direction a b c : turns (b : c : rest)


reflection1 :: Num a => [Point a] -> [Point a]
reflection1 =
    map (\(a1, a2) -> (a1, (-a2)))


