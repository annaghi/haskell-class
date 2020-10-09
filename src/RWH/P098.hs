{-# LANGUAGE ScopedTypeVariables #-}

module RWH.P098
    ( concatFoldr
    , takeWhileRecursive
    , takeWhileFoldr
    , groupByRecursive
    , groupByFoldr
    , groupByFoldr2
    , groupByFoldl
    , anyFoldr
    , cycleFoldr
    , wordsFoldr
    , unlinesFoldr
    ) where

import Data.List (foldl')
import Text.Latin1 (isWhiteSpace)


concatFoldr :: [[a]] -> [a]
concatFoldr =
    foldr (++) []


takeWhileRecursive :: (a -> Bool) -> [a] -> [a]
takeWhileRecursive _ [] = []
takeWhileRecursive p (x:ys) =
    if p x then x : takeWhileRecursive p ys else []


{-
https://www.cs.nott.ac.uk/~pszgmh/fold.pdf

Transform a primitive recursive function to foldr

takeWhile _ []     = z
takeWhile p (x:ys) = f x (takeWhile p ys)      ⟺      takeWhile p = foldr f z


takeWhile _ [] = z
⟺ takeWhile _ [] = []
⟹ z = []

takeWhile p (x:ys) = f x (takeWhile p ys)
⟺ if p x then x : takeWhile p ys else [] = f x (takeWhile p ys)     // as := takeWhile p ys
⟺ if p x then x : as else [] = f x as
⟹ f = \x as -> if p x then x : as else []
-}
takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr _ [] = []
takeWhileFoldr p xs =
    foldr (\x as -> if p x then x : as else []) [] xs


groupByRecursive :: (a -> a -> Bool) -> [a] -> [[a]]
groupByRecursive _ [] = []
groupByRecursive p (x:ys) =
    (x : prefix) : groupByRecursive p remainder
    where
        (prefix, remainder) = span (p x) ys


groupByFoldr :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldr p =
    foldr
        (\e a ->
            if null a then
                [[e]]
            else if p e (head $ head a) then
                (e : head a) : (tail a)
            else
                [e] : a
        ) []


groupByFoldr2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldr2 _ [] = []
groupByFoldr2 p ls =
    foldr
        (\e ((x:ys):as) ->
            if p e x then
                (e : x : ys) : as
            else
                [e] : ((x : ys) : as)
        ) [[(last ls)]] (init ls)


groupByFoldl :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldl _ [] = []
groupByFoldl p (h:ts) =
        reverse
        $ foldl'
            (\((x:ys):as) e ->
                if p e x then
                    (e : x : ys) : as
                else
                    [e] : ((x : ys) : as)
            ) [[h]] ts


anyFoldr :: (a -> Bool) -> [a] -> Bool
anyFoldr p =
    foldr (\e a -> p e || a) False


cycleFoldr :: [a] -> [a]
cycleFoldr [] = []
cycleFoldr xs =
    foldr (\(_ :: Integer) a -> xs ++ a) [] [1..]


wordsFoldr :: String -> [String]
wordsFoldr "" = []
wordsFoldr xs =
    foldr
        (\e (x:as) ->
            if isWhiteSpace e then
                "" : x : as
            else
                (e : x) : as
        ) [""] xs


unlinesFoldr :: [String] -> String
unlinesFoldr =
    foldr (\e a -> e ++ '\n' : a) ""
