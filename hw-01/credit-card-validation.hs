{-# OPTIONS_GHC -Wall #-}

module CreditCardValidation
    ( validate
    , validateTest
    ) where


-- https://en.wikipedia.org/wiki/Luhn_algorithm


import Data.Char (digitToInt)


validate :: String -> String
validate xs =
    let
        xs' = clean xs
    in
    if length xs == length xs'
        then output $ checkIfZero $ remainderBy10 $ sumDigits $ doubleEveryOther xs'
        else "The input is invalid"


clean :: String -> [Int]
clean xs =
    [ digitToInt x | x <- xs, x `elem` ['0'..'9'] ]


doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs =
    reverse [ if odd i then 2 * x else x | (i, x) <- zip ([0..] :: [Int]) (reverse xs) ]


sumDigits :: [Int] -> Int
sumDigits xs =
    sum [ digitToInt x | x <- concat [ show x | x <- xs ] ]


remainderBy10 :: Int -> Int
remainderBy10 =
    (`rem` 10)


checkIfZero :: Int -> Bool
checkIfZero =
    (== 0)


output :: Bool -> String
output b =
    if b
        then "Valid!"
        else "The card number is invalid"


-- TESTS

validateTest :: Bool
validateTest = and
    [ validate "4012888888881881" == "Valid!"
    , validate "4012888888881882" == "The card number is invalid"
    , validate "1234abcd5678efgh" == "The input is invalid"
    ]
