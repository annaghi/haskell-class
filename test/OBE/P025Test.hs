module OBE.P025Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog (MonadGen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Applicative (liftA3)
import Control.Lens.Combinators (set, view)

import qualified OBE.P025 as EE


tests :: TestTree
tests =
    testGroup "Page 25"
        [ testGroup "Exercise 1: second"
            [ testProperty "Law: set-get" prop_set_get
            , testProperty "Law: get-set" prop_get_set
            , testProperty "Law: set-set" prop_set_set       
            ]
        ]



-- Properties


prop_set_get :: Property
prop_set_get =
    property $ do
        structure <- forAll genIntTriplet
        newValue <- forAll genInt
        view EE.second (set EE.second newValue structure) === newValue


prop_get_set :: Property
prop_get_set =
    property $ do
        structure <- forAll genIntTriplet
        set EE.second (view EE.second structure) structure === structure


prop_set_set :: Property
prop_set_set =
    property $ do
        structure <- forAll genIntTriplet
        newValue <- forAll genInt
        differentValue <- forAll genInt
        set EE.second differentValue (set EE.second newValue structure) === set EE.second differentValue structure



-- Generators


genInt :: MonadGen m => m Int
genInt =
    Gen.int (Range.linearFrom 0 (-50) 50)


genIntTriplet :: MonadGen m => m (Int, Int, Int)
genIntTriplet =
    liftA3 (,,) genInt genInt genInt
