module RWH.P097Test (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils ((|>))
import           Test.Hspec.Expectations (Selector, shouldThrow, errorCall)
import           Control.Exception (ErrorCall(ErrorCall), evaluate)
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Char (digitToInt, intToDigit)

import qualified RWH.P097 as EE


tests :: TestTree
tests =
    testGroup "Page 97"
        [ testGroup "Exercise 1"
            [ testCase "when the string is 0" $
                EE.asInt_fold_1 "0"
                    |> assertEqual "" 0
            , testCase "when the string is maxBound" $
                (EE.asInt_fold_1 $ show (maxBound :: Int))
                    |> assertEqual "" (maxBound :: Int)
            , testProperty "when the string is non-negative" $
                property $ do
                    xs <- forAll genNonNegativeInt
                    EE.asInt_fold_1 (show (xs :: Int)) === xs
            ]
        , testGroup "Exercise 2"
            [ testCase "when the string is minBound" $
                (EE.asInt_fold_2 $ show (minBound :: Int))
                    |> assertEqual "" (minBound :: Int)
            , testCase "when the string is maxBound" $
                (EE.asInt_fold_2 $ show (maxBound :: Int))
                    |> assertEqual "" (maxBound :: Int)
            , testCase "when the string is 0" $
                EE.asInt_fold_2 "0"
                    |> assertEqual "" 0
            , testProperty "when the string is valid number" $
                property $ do
                    xs <- forAll genInt
                    EE.asInt_fold_2 (show (xs :: Int)) === xs
            ]
        , testGroup "Exercise 3"
            [ testCase "when the string is empty" $ do
                evaluate (EE.asInt_fold_3 "")
                    `shouldThrow` exception
            , testCase "when the string is just an - " $ do
                evaluate (EE.asInt_fold_3 "-")
                    `shouldThrow` exception
            , testCase "when the string is contains invalid characters" $ do
                evaluate (EE.asInt_fold_3 "343.4")
                    `shouldThrow` exception
            , testCase "when the string is less than minBound" $ do
                evaluate (EE.asInt_fold_3 $ addOneToLastDigit $ show (minBound :: Int))
                    `shouldThrow` exception
            , testCase "when the string is greater than maxBound" $ do
                evaluate (EE.asInt_fold_3 $ addOneToLastDigit $ show (maxBound :: Int))
                    `shouldThrow` exception
            , testProperty "when the string is valid number" $
                property $ do
                    xs <- forAll genInt
                    EE.asInt_fold_3 (show (xs :: Int)) === xs
            ]
        , testGroup "Exercise 4"
            [ testCase "when the string is empty" $
                EE.asInt_either ""
                    |> assertEqual "" (Left "empty string")
            , testCase "when the string is just an - " $
                EE.asInt_either "-"
                    |> assertEqual "" (Left "the string is just a minus sign")
            , testCase "when the string is contains invalid characters" $
                EE.asInt_either "343.4"
                    |> assertEqual "" (Left "the string contains non-digits")
            , testCase "when the string is less than minBound" $
                (EE.asInt_either $ addOneToLastDigit $ show (minBound :: Int))
                    |> assertEqual "" (Left "the string is less than minBound")
            , testCase "when the string is greater than maxBound" $
                (EE.asInt_either $ addOneToLastDigit $ show (maxBound :: Int))
                    |> assertEqual "" (Left "the string is greater than maxBound")
            , testProperty "when the string is valid number" $
                property $ do
                    xs <- forAll genInt
                    EE.asInt_either (show (xs :: Int)) === Right xs
            ]
        ]



-- Generators


genNonNegativeInt :: MonadGen m => m Int
genNonNegativeInt =
    Gen.int (Range.linear 0 (maxBound :: Int))


genInt :: MonadGen m => m Int
genInt =
    Gen.int (Range.linear (minBound :: Int) (maxBound :: Int))



-- Helpers


exception :: Selector ErrorCall
exception =
    errorCall "Invalid input string, cannot accept as a number"


addOneToLastDigit :: String -> String
addOneToLastDigit xs =
    init xs ++ [intToDigit $ (+ 1) $ digitToInt $ last xs]
