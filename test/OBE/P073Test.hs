module OBE.P073Test (tests) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Hedgehog (MonadGen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Optics.Lens (Lens')
import Optics.Getter (view)
import Optics.Setter (set)
import Optics.Optic ((%))

import qualified OBE.P073 as EE


tests :: TestTree
tests =
    testGroup "Page 73"
        [ testGroup "when composing lenses"
            [ testProperty "Law: set-get" (prop_set_get (EE.army % EE.archers))
            , testProperty "Law: get-set" (prop_get_set (EE.army % EE.archers))
            , testProperty "Law: set-set" (prop_set_set (EE.army % EE.archers))
            ]
        , testGroup "composing lenses using functions"
            [ testProperty "should be the same as using operators, goalA" $
                property $ do
                    kingdom <- forAll genKingdom
                    EE.goalA_Fn kingdom === EE.goalA_Op kingdom
            , testProperty "should be the same as using operators, goalB" $
                property $ do
                    kingdom <- forAll genKingdom
                    EE.goalB_Fn kingdom === EE.goalB_Op kingdom
            , testProperty "should be the same as using operators, goalC" $
                property $ do
                    kingdom <- forAll genKingdom
                    EE.goalC_Fn kingdom === EE.goalC_Op kingdom
            , testProperty "should be the same as using state operators, goalC" $
                property $ do
                    kingdom <- forAll genKingdom
                    EE.goalC_Fn kingdom === EE.goalC_Op_State kingdom
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
