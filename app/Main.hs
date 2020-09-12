module Main where

import RWH.P060
import RWH.P069
import RWH.P070

import HW01.CreditCardValidation
import HW01.TowerOfHanoi

import System.Random


main :: IO ()
main = do
    r <- (randomIO :: IO Int)
    print $ RWH.P060.test
    print $ RWH.P069.test r
    print $ RWH.P070.test
    print $ HW01.CreditCardValidation.test
    print $ HW01.TowerOfHanoi.test
