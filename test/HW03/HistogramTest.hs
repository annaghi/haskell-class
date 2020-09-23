module HW03.HistogramTest (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils ((|>))

import qualified HW03.Golf as EE


tests :: TestTree
tests =
    testGroup "Exercise 3: Histogram"
        [ testCase "when the list is empty" $
            EE.histogram []
                |> assertEqual "" "==========\n0123456789\n"
        ]
