module OBE.Test (suite) where

import Test.Tasty

import OBE.P025Test
import OBE.P036Test
import OBE.P041Test


suite :: TestTree
suite =
    testGroup "OBE"
        [ OBE.P025Test.tests
        , OBE.P036Test.tests
        , OBE.P041Test.tests
        ]
