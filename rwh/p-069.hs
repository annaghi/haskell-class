{-# OPTIONS_GHC -Wall #-}

module Page69
    ( length'
    , mean
    , median
    , averages
    , test
    ) where


import Data.List (sort)


length' :: Num b => [a] -> b
length' []     = 0
length' (_:xs) = 1 + length' xs


mean :: Fractional a => [a] -> a
mean xs = sum xs / length'' xs
    where
        length'' :: Num b => [a] -> b
        length'' = fromIntegral . length


-- This function is taken from Numeric.Statistics.Median
median :: (Ord a, Fractional a) => [a] -> a
median xs =
    if odd n
        then sort xs !! (n `div` 2)
        else ((sort xs !! (n `div` 2 - 1)) + (sort xs !! (n `div` 2))) / 2
    where
        n :: Int
        n = length xs


averages :: [[a] -> a] -> [a] -> [a]
averages _ [] = []
averages [] _ = []
averages fs xs = map ($ xs) fs


-- TESTS

test :: Bool
test = and
    [ length' []                      == length []
    , length' "abcde"                 == length "abcde"
    , (isNaN $ (mean [] :: Double))   == True
    , mean [1,2,6]                    == (3.0 :: Double)
    , averages [mean] []              == ([] :: [Double])
    , averages [] [1,2,6]             == ([] :: [Double])
    , averages [mean, median] [1,2,6] == [3.0 :: Double, 2.0 :: Double]
    ]
