{-# OPTIONS_GHC -Wall #-}

module TowerOfHanoi
    ( hanoi
    , hanoi4
    , hanoiN
    , hanoiTest
    ) where


-- https://en.wikipedia.org/wiki/Tower_of_Hanoi
-- http://service.scs.carleton.ca/sites/default/files/tr/TR-04-10.pdf

-- Origin is the first peg
-- Destination is the second peg
-- Peg is an Int so we can compare the results simply by eye


type Peg
    = Int

type Move
    = (Peg, Peg)


hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 o d _ = [(o, d)]
hanoi n o d t = hanoi (n - 1) o t d ++ hanoi 1 o d t ++ hanoi (n - 1) t d o


hanoi4 :: Int -> Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 _ o d _ _ = [(o, d)]
hanoi4 n k o d t t' = hanoi4 (n - k) k o t t' d ++ hanoi k o d t' ++ hanoi4 (n - k) k t d o t'


hanoiN :: Int -> [Peg] -> [Move]
hanoiN 1 (o:d:_) = [(o, d)]
hanoiN n (o:d:t:ts) = hanoiN k (o:t:d:ts) ++ hanoiN (n - k) (o:d:ts) ++ hanoiN k (t:d:o:ts)
    where k
            | null ts   = n - 1
            | otherwise = n - round ((sqrt :: Double -> Double) (fromIntegral (2 * n + 1))) + 1
hanoiN _ _ = []


-- TESTS

hanoiTest :: Bool
hanoiTest = and
    [ hanoi 3 1 2 3         == [(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2)]
    , hanoiN 3 [1, 2, 3]    == [(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2)]
    , hanoi4 3 2 1 2 3 4    == [(1,3),(1,4),(1,2),(4,2),(3,2)]
    , hanoiN 3 [1, 2, 3, 4] == [(1,3),(1,4),(1,2),(4,2),(3,2)]
    , hanoi4 4 3 1 2 3 4    == [(1,3),(1,2),(1,4),(2,4),(1,2),(4,1),(4,2),(1,2),(3,2)]
    , hanoiN 4 [1, 2, 3, 4] == [(1,2),(1,3),(2,3),(1,4),(1,2),(4,2),(3,1),(3,2),(1,2)]
    , hanoi4 4 1 1 2 3 4    == [(1,2),(1,4),(2,4),(1,3),(4,1),(4,3),(1,3),(1,2),(3,4),(3,1),(4,1),(3,2),(1,3),(1,2),(3,2)]
    ]
