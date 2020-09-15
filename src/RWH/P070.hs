module RWH.P070
    ( sortByLength
    , intersperse
    , Tree(..)
    , height
    , count
    , Point(..)
    , Direction(..)
    , direction
    , oppositeDirection
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


data Point =
    Point Float Float
    deriving (Show)


data Direction
    = Left
    | Right
    | Straight
    deriving (Show, Eq)


direction :: Point -> Point -> Point -> Direction
direction (Point a1 a2) (Point b1 b2) (Point c1 c2) =
    if determinant > 0 then
        Left
    else if determinant < 0 then
        Right
    else
        Straight
    where
        determinant :: Float
        determinant = ((b1 - a1) * (c2 - a2)) - ((b2 - a2) * (c1 - a1))


oppositeDirection :: Direction -> Direction
oppositeDirection direction =
    case direction of
        Left ->
            Right

        Right ->
            Left

        Straight ->
            Straight
