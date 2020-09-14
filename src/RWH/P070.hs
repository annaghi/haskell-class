module RWH.P070
    ( sortByLength
    , intersperse
    , Tree(..)
    , height
    , count
    ) where

import Data.List (sortBy)
import Data.Function (on)


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
