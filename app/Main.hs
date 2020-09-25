module Main where

import RWH.P060
import RWH.P069
import RWH.P084

import HW01.CreditCardValidation
import HW01.TowerOfHanoi

import System.Random


main :: IO ()
main =
    do
    randomInt <- (randomIO :: IO Int)
    print RWH.P060.test
    print $ RWH.P069.test randomInt
    print HW01.CreditCardValidation.test
    print HW01.TowerOfHanoi.test
    processFile RWH.P084.firstWords "data/RWH/input-084.txt" "data/RWH/output-084-3.txt"
    processFile RWH.P084.transposeText "data/RWH/input-084.txt" "data/RWH/output-084-4.txt"


processFile :: (String -> String) -> FilePath -> FilePath -> IO ()
processFile f inputFile outputFile =
    do
    input <- readFile inputFile
    writeFile outputFile (f input)
