module RWH.P097
    ( asInt_fold_1
    , asInt_fold_2
    , asInt_fold_3
    , asInt_either
    ) where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')


asInt_fold_1 :: String -> Int
asInt_fold_1 =
    foldl' (\a e -> a * 10 + digitToInt e) 0


asInt_fold_2 :: String -> Int
asInt_fold_2 "" = 0
asInt_fold_2 ls@(x:xs)
    | x == '-' =
        (-1) * asInt_fold_1 xs
    | otherwise =
        asInt_fold_1 ls


asInt_fold_3 :: String -> Int
asInt_fold_3  "" = raiseError
asInt_fold_3 "-" = raiseError
asInt_fold_3 ls@(x:xs)
    | (x /= '-' && (not . isDigit) x) || any (not . isDigit) xs =
        raiseError
    | x == '-' =
        if lessThanMinBound xs then
            raiseError
        else
            (-1) * asInt_fold_1 xs
    | otherwise =
        if greaterThanMaxBound ls then
            raiseError
        else
            asInt_fold_1 ls


type ErrorMessage =
    String


asInt_either :: String -> Either ErrorMessage Int
asInt_either  "" = Left "empty string"
asInt_either "-" = Left "the string is just a minus sign"
asInt_either ls@(x:xs)
    | (x /= '-' && (not . isDigit) x) || any (not . isDigit) xs =
        Left "the string contains non-digits"
    | x == '-' =
        if lessThanMinBound xs then
            Left "the string is less than minBound"
        else
            Right $ (-1) * asInt_fold_1 xs
    | otherwise =
        if greaterThanMaxBound ls then
            Left "the string is greater than maxBound"
        else
            Right $ asInt_fold_1 ls



raiseError :: Int
raiseError = 
    error "Invalid input string, cannot accept as a number"


lessThanMinBound :: String -> Bool
lessThanMinBound =
    outOfBound (drop 1 $ show (minBound :: Int))


greaterThanMaxBound :: String -> Bool
greaterThanMaxBound =
    outOfBound (show (maxBound :: Int))


outOfBound :: String -> String -> Bool
outOfBound bound xs
    | length xs > length bound =
        True
    | length xs < length bound || xs == bound =
        False
    | otherwise =
        foldr (\(b, x) a -> x >= b && a) True $ zip bound xs
