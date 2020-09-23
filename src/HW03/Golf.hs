module HW03.Golf
    ( skips
    , localMaxima
    , histogram
    ) where


import Data.Tuple.Extra (uncurry3)
import Data.Maybe (mapMaybe)
import qualified Data.MultiSet as MS
import qualified Data.Map as M


skips :: [a] -> [[a]]
skips xs =
    [concat [[x !! ((s - 1) * (n + 1) + n) | x <- [xs]] | n <- [0..length xs `div` s - 1]] | s <- [1..length xs]]


localMaxima :: [Integer] -> [Integer]
localMaxima =
    mapMaybe (uncurry3 (\x y z -> if y > x && y > z then Just y else Nothing))
    . (zip3 <*> tail <*> tail . tail)


histogram :: [Integer] -> String
histogram xs =
    unlines
    $ reverse
    $ (:) "0123456789"
    $ (:) "=========="
    $ takeWhile (/= "          ")
    $ map (concatMap (\v -> if v > 0 then "*" else " "))
    $ iterate (map (subtract 1))
    $ map snd
    $ M.toList
    $ flip M.union (M.fromList $ zip [0..9] (repeat 0))
    $ MS.toMap
    $ MS.fromList
    $ filter (\v -> 0 <= v && v <= 9)
      xs
