module HW04.Test (suite) where

import Test.Tasty

import HW04.E1Test
import HW04.E3Test


suite :: TestTree
suite =
    testGroup "Homework 4"
        [ HW04.E1Test.tests
        , HW04.E3Test.tests
        ]
