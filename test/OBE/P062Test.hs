module OBE.P062Test (tests) where

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Optics.Lens (Lens')
import Optics.Getter (view)
import Optics.Setter (set)
import Optics.Optic ((%))

import qualified OBE.P062 as EE


tests :: TestTree
tests =
    testGroup "Page 62"
        [ testGroup "Kingdom with functions"
            [ testProperty "Law: set-get" (prop_set_get (EE.army % EE.archers))
            , testProperty "Law: get-set" (prop_get_set (EE.army % EE.archers))
            , testProperty "Law: set-set" (prop_set_set (EE.army % EE.archers))
            ]
        ]



-- Properties


prop_set_get :: Lens' EE.Kingdom Int -> Property
prop_set_get lens =
    property $ do
        structure <- forAll genKingdom
        newValue <- forAll genInt
        view lens (set lens newValue structure) === newValue


prop_get_set :: Lens' EE.Kingdom Int -> Property
prop_get_set lens =
    property $ do
        structure <- forAll genKingdom
        set lens (view lens structure) structure === structure


prop_set_set :: Lens' EE.Kingdom Int -> Property
prop_set_set lens =
    property $ do
        structure <- forAll genKingdom
        newValue <- forAll genInt
        differentValue <- forAll genInt
        set lens differentValue (set lens newValue structure) === set lens differentValue structure



-- Generators


genKingdom :: MonadGen m => m EE.Kingdom
genKingdom =
    EE.Kingdom <$> genString <*> genArmy <*> genGate


genArmy :: MonadGen m => m EE.Army
genArmy =
    EE.Army <$> genInt <*> genInt


genGate :: MonadGen m => m EE.Gate
genGate =
    EE.Gate <$> Gen.bool_ <*> genFloat


genFloat :: MonadGen m => m Float
genFloat =
    Gen.float (Range.linearFrac (-50) 50)


genInt :: MonadGen m => m Int
genInt =
    Gen.int (Range.linear 0 50)


genString :: MonadGen m => m String
genString =
    Gen.list (Range.linear 0 50) Gen.alpha
