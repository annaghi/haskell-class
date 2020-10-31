module RWH.Test (suite) where

import Test.Tasty (TestTree, testGroup)

import RWH.P070Test
import RWH.P084Test
import RWH.P097Test
import RWH.P098Test


suite :: TestTree
suite =
    testGroup "RWH"
        [ RWH.P070Test.tests
        , RWH.P084Test.tests
        , RWH.P097Test.tests
        , RWH.P098Test.tests
        ]
