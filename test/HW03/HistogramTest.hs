module HW03.HistogramTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Utils ((|>))

import qualified HW03.Golf as EE


tests :: TestTree
tests =
    testGroup "Exercise 3: Histogram"
        [ testCase "when the list is empty" $
            EE.histogram []
                |> assertEqual "" "==========\n0123456789\n"
        , testCase "when the list is [1,1,1,5]" $
            EE.histogram [1,1,1,5]
                |> assertEqual "" " *        \n *        \n *   *    \n==========\n0123456789\n"
        , testCase "when the list is [1,4,5,4,6,6,3,4,2,4,9]" $
            EE.histogram [1,4,5,4,6,6,3,4,2,4,9]
                |> assertEqual "" "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"
        ]
