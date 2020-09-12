module RWH.P070
    ( sortByLength
    ) where


import Data.List (sortBy)
import Data.Function (on)


sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (compare `on` length)
