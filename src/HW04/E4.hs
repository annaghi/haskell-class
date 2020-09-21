module HW04.E4 ( sieveSundaram ) where


-- https://en.wikipedia.org/wiki/Sieve_of_Sundaram


import Data.List ((\\))


sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    map ((+1) . (*2))
    $ ((\\) [1..n])
    $ [i + j + 2 * i * j | j <- [1..n], i <- [1..j]]
