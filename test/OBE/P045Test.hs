module OBE.P045Test (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils ((|>))
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Optics.Lens (Lens')
import Optics.Getter (view)
import Optics.Setter (set)

import qualified OBE.P045 as EE


tests :: TestTree
tests =
    testGroup "Page 45"
        [ testGroup "Exercise 1-2"
            [ testCase "when the limePrice - lemonPrice > 0.50" $
                (view EE.lemonPrice $ set EE.limePrice 2 (EE.ProducePrices 1.50 1.48))
                    |> assertEqual "" 1.5
            , testCase "when the limePrice - lemonPrice < 0.50" $
                (view EE.lemonPrice $ set EE.limePrice 1.8 (EE.ProducePrices 1.50 1.48))
                    |> assertEqual "" 1.48
            , testCase "when the limePrice < 0" $
                (view EE.lemonPrice $ set EE.limePrice (-1) (EE.ProducePrices 1.50 1.48))
                    |> assertEqual "" 0.5
            -- All laws are failing
            -- , testProperty "Law: set-get" (prop_set_get EE.limePrice)
            -- , testProperty "Law: get-set" (prop_get_set EE.limePrice)
            -- , testProperty "Law: set-set" (prop_set_set EE.limePrice)
            ]
        ]



-- Properties


prop_set_get :: Lens' EE.ProducePrices Float -> Property
prop_set_get lens =
    property $ do
        structure <- forAll genProducePrices
        newValue <- forAll genFloat
        view lens (set lens newValue structure) === newValue


prop_get_set :: Lens' EE.ProducePrices Float -> Property
prop_get_set lens =
    property $ do
        structure <- forAll genProducePrices
        set lens (view lens structure) structure === structure


prop_set_set :: Lens' EE.ProducePrices Float -> Property
prop_set_set lens =
    property $ do
        structure <- forAll genProducePrices
        newValue <- forAll genFloat
        differentValue <- forAll genFloat
        set lens differentValue (set lens newValue structure) === set lens differentValue structure



-- Generators


genFloat :: MonadGen m => m Float
genFloat =
    Gen.float (Range.linearFrac (-50) 50)


genProducePrices :: MonadGen m => m EE.ProducePrices
genProducePrices =
    EE.ProducePrices <$> genFloat <*> genFloat
