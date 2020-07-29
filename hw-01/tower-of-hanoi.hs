-- https://en.wikipedia.org/wiki/Tower_of_Hanoi
-- http://service.scs.carleton.ca/sites/default/files/tr/TR-04-10.pdf


type Peg = String

type Move = (Peg, Peg)


-- Destination is the last peg
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a _ c = [(a, c)]
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) b a c


-- Destination is the last peg
hanoi4 :: Int -> Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 _ a _ _ d = [(a, d)]
hanoi4 n k a b c d = hanoi4 (n - k) k a c d b ++ hanoi k a c d ++ hanoi4 (n - k) k b a c d


-- Destination is the second peg, works when the number of the pegs is greater than 3
hanoiN :: Int -> [Int] -> [(Int, Int)]
hanoiN 1 (x:y:rest) = [(x, y)]
hanoiN n (x:y:z:rest) =
    hanoiN k (x:z:y:rest)
    ++
    hanoiN (n - k) (x:y:rest)
    ++
    hanoiN k (z:y:x:rest)
    where k = n - round (sqrt (fromIntegral (2 * n + 1))) + 1
