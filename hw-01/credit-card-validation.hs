-- https://en.wikipedia.org/wiki/Luhn_algorithm


import Data.Char


validate :: String -> String
validate xs =
    let
        xs' = clean xs
    in
    if length xs == length xs'
        then output $ checkIfZero $ remainderBy10 $ sumDigits $ doubleEverySecond xs'
        else "The input is invalid"


clean :: String -> [Int]
clean xs =
    [ digitToInt x | x <- xs, x `elem` ['0'..'9'] ]


doubleEverySecond :: [Int] -> [Int]
doubleEverySecond xs =
    reverse $ [ doubleIfIndexIsOdd i x | (i, x) <- zip [0..] (reverse xs) ]


doubleIfIndexIsOdd :: Int -> Int -> Int
doubleIfIndexIsOdd i x
    | odd i     = 2 * x
    | otherwise = x


sumDigits :: [Int] -> Int
sumDigits xs =
    sum [ digitToInt x | x <- concat [ show x | x <- xs ] ]


remainderBy10 :: Int -> Int
remainderBy10 x =
    x `rem` 10


checkIfZero :: Int -> Bool
checkIfZero =
    (==) 0


output :: Bool -> String
output b =
    if b
        then "Valid!"
        else "The card number is invalid"
