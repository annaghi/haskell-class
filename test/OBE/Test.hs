module OBE.Test (suite) where

import Test.Tasty (TestTree, testGroup)

import OBE.P025Test
import OBE.P036Test
import OBE.P041Test
import OBE.P045Test
import OBE.P073Test


suite :: TestTree
suite =
    testGroup "OBE"
        [ OBE.P025Test.tests
        , OBE.P036Test.tests
        , OBE.P041Test.tests
        , OBE.P045Test.tests
        , OBE.P073Test.tests
        ]
