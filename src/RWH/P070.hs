{-# LANGUAGE ScopedTypeVariables #-}

module RWH.P070
    ( sortByLength
    , intersperse
    , Tree(..)
    , height
    , count
    , Point
    , Direction(..)
    , turn
    , oppositeDirection
    , turns
    , reflection1
    , sortByAngleWithP0
    , sortByAngleWithP0'
    , travel
    , travel'
    , convexHull
    , convexHull'
    ) where


-- https://en.wikipedia.org/wiki/Graham_scan


import Data.List (sortBy)
import Data.Function (on)
import GHC.Exts (sortWith)
import Data.Tuple (swap)
import Prelude hiding (Left, Right)


sortByLength :: [[a]] -> [[a]]
sortByLength =
    sortBy (compare `on` length)


intersperse :: forall a. a -> [[a]] -> [a]
intersperse separator =
    go
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


turn :: forall a. (Num a, Ord a) => Point a -> Point a -> Point a -> Direction
turn (a1, a2) (b1, b2) (c1, c2) =
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
oppositeDirection direction =
    case direction of
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
turns (a:b:c:rest) =
    turn a b c : turns (b : c : rest)


reflection1 :: Num a => [Point a] -> [Point a]
reflection1 =
    map (\(a1, a2) -> (a1, (-a2)))


convexHull :: (Floating a, Ord a) => [Point a] -> [Point a]
convexHull []     = []
convexHull [_]    = []
convexHull [_,_]  = []
convexHull points =
    travel
    $ (\ps -> sortByAngleWithP0 (head ps) (tail ps))
    $ sortWith swap points


sortByAngleWithP0 :: (Floating a, Ord a) => Point a -> [Point a] -> [Point a]
sortByAngleWithP0 p0 points =
    p0 : (sortBy (compare `on` (negate . cosinus (1,0) . translate p0)) points)


travel :: (Num a, Ord a) => [Point a] -> [Point a]
travel  =
    foldl (\acc p -> 
        case acc of
            [] ->
                [p]

            [a] ->
                [p,a]

            [a,b] ->
                [p,a,b]

            (a:b:rest) ->
                case turn b a p of
                    Left ->
                        p : acc

                    _ ->
                        p : b : rest
    ) []


cosinus :: Floating a => Point a -> Point a -> a
cosinus (a1, a2) (b1, b2) =
    (a1 * b1 + a2 * b2) / ((sqrt (a1 * a1 + a2 * a2)) * (sqrt (b1 * b1 + b2 * b2)))


translate :: Num a => Point a -> Point a -> Point a
translate (a1, a2) (b1, b2) =
    (b1 - a1, b2 - a2)


convexHull' :: (Floating a, Ord a) => [Point a] -> [Point a]
convexHull' []     = []
convexHull' [_]    = []
convexHull' [_,_]  = []
convexHull' points =
    travel'
    $ (\ps -> sortByAngleWithP0' (head ps) (tail ps))
    $ reverse
    $ sortWith swap points


sortByAngleWithP0' :: (Floating a, Ord a) => Point a -> [Point a] -> [Point a]
sortByAngleWithP0' p0 points =
    p0 : (sortBy (compare `on` (negate . cosinus (1,0) . translate p0)) points)


travel' :: (Num a, Ord a) => [Point a] -> [Point a]
travel'  =
    foldl (\acc p -> 
        case acc of
            [] ->
                [p]

            [a] ->
                [p,a]

            [a,b] ->
                [p,a,b]

            (a:b:rest) ->
                case turn b a p of
                    Right ->
                        p : acc

                    _ ->
                        p : b : rest
                
    ) []
