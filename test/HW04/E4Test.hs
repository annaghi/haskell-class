module HW04.E4Test (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils ((|>))

import qualified HW04.E4 as EE


tests :: TestTree
tests =
    testGroup "Exercise 4"
        [ testCase "sieveSundaram" $
            EE.sieveSundaram 100
                |> assertEqual "" [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199]
        ]
