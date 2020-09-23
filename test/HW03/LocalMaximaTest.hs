module HW03.LocalMaximaTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Utils ((|>))

import qualified HW03.Golf as EE


tests :: TestTree
tests =
    testGroup "Exercise 2: Local maxima"
        [ testCase "when the list is empty" $
            EE.localMaxima []
                |> assertEqual "" []
        , testCase "when the list has one element" $
            EE.localMaxima [1]
                |> assertEqual "" []
        , testCase "when the list has two elements" $
            EE.localMaxima [1,4]
                |> assertEqual "" []
        , testCase "when the list is [2,9,5,6,1]" $
            EE.localMaxima [2,9,5,6,1]
                |> assertEqual "" [9,6]
        , testCase "when the list is [2,3,4,1,5]" $
            EE.localMaxima [2,3,4,1,5]
                |> assertEqual "" [4]
        , testCase "when the list is [1,2,3,4,5]" $
            EE.localMaxima [1,2,3,4,5]
                |> assertEqual "" []
        ]
