module HW03.Test (suite) where

import Test.Tasty

import HW03.SkipsTest
import HW03.LocalMaximaTest
import HW03.HistogramTest


suite :: TestTree
suite =
    testGroup "Homework 3"
        [ HW03.SkipsTest.tests
        , HW03.LocalMaximaTest.tests
        , HW03.HistogramTest.tests
        ]
