module OBE.P036Test (tests) where

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Lens.Combinators (set, view)

import qualified OBE.P036 as EE


tests :: TestTree
tests =
    testGroup "Page 36"
        [ testGroup "Exercise 6: builder"
            [ testProperty "Law: set-get" prop_set_get
            , testProperty "Law: get-set" prop_get_set
            , testProperty "Law: set-set" prop_set_set       
            ]
        ]



-- Properties


prop_set_get :: Property
prop_set_get =
    property $ do
        structure <- forAll genBuilder
        newValue <- forAll genString
        view EE.builder (set EE.builder newValue structure) === newValue


prop_get_set :: Property
prop_get_set =
    property $ do
        structure <- forAll genBuilder
        set EE.builder (view EE.builder structure) structure === structure


prop_set_set :: Property
prop_set_set =
    property $ do
        structure <- forAll genBuilder
        newValue <- forAll genString
        differentValue <- forAll genString
        set EE.builder differentValue (set EE.builder newValue structure) === set EE.builder differentValue structure



-- Generators


genString :: MonadGen m => m String
genString =
    Gen.list (Range.linear 1 50) Gen.alpha


genStrings :: MonadGen m => m [String]
genStrings =
    Gen.list (Range.linear 0 10) genString


genBuilder :: MonadGen m => m EE.Builder
genBuilder =
    fmap (\context -> EE.Builder context unwords words) genStrings
