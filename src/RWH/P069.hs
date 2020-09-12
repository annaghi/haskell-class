{-# OPTIONS_GHC -Wall #-}

module RWH.P069
    ( length'
    , mean
    , averages
    , toPalindrome
    , isPalindrome
    , test
    ) where


import Numeric.Statistics.Median (median)


length' :: Num b => [a] -> b
length' []     = 0
length' (_:xs) = 1 + length' xs


mean :: Fractional a => [a] -> a
mean xs = sum xs / length'' xs
    where
        length'' :: Num b => [a] -> b
        length'' = fromIntegral . length


averages :: [[a] -> a] -> [a] -> [a]
averages _ [] = []
averages [] _ = []
averages fs xs = map ($ xs) fs


toPalindrome :: Int -> [a] -> [a]
toPalindrome random xs =
    if random `mod` 2 == 0
        then xs ++ (reverse xs)
        else xs ++ (tail $ reverse xs)


isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs =
    xs == reverse xs


-- TESTS

test :: Int -> Bool
test random = and
    [ length' []                      == length []
    , length' "abcde"                 == length "abcde"
    , (isNaN (mean [] :: Double))     == True
    , mean [1,2,6]                    == (3.0 :: Double)
    , averages [mean] []              == ([] :: [Double])
    , averages [] [1,2,6]             == ([] :: [Double])
    , averages [mean, median] [1,2,6] == [3.0 :: Double, 2.0 :: Double]
    , (toPalindrome random "an") `elem` ["ana", "anna"]
    , isPalindrome "palindrome"       == False
    , isPalindrome "kazak"            == True
    ]
