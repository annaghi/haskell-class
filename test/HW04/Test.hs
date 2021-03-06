module HW04.Test (suite) where

import Test.Tasty (TestTree, testGroup)

import HW04.E1Test
import HW04.E2Test
import HW04.E3Test
import HW04.E4Test


suite :: TestTree
suite =
    testGroup "Homework 4"
        [ HW04.E1Test.tests
        , HW04.E2Test.tests
        , HW04.E3Test.tests
        , HW04.E4Test.tests
        ]
