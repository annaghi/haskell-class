{-# LANGUAGE RankNTypes #-}

module OBE.P041Test (tests) where

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Optics.Lens (Lens')
import Optics.Getter (view)
import Optics.Setter (set)
import Control.Applicative (liftA2)

import qualified OBE.P041 as EE


tests :: TestTree
tests =
    testGroup "Page 41"
        [ testGroup "Exercise 1: check firstName"
            [ testProperty "Law: set-get" (prop_set_get EE.firstName)
            , testProperty "Law: get-set" (prop_get_set EE.firstName)
            , testProperty "Law: set-set" (prop_set_set EE.firstName)
            ]
        , testGroup "Exercise 1: delete _userName"
            [ testProperty "Law: set-get" (prop_set_get EE.userName)
            , testProperty "Law: get-set" (prop_get_set EE.userName)
            , testProperty "Law: set-set" (prop_set_set EE.userName)
            ]
        , testGroup "Exercise 2: add fullName"
            [ testProperty "Law: set-get" (prop_set_get EE.fullName)
            , testProperty "Law: get-set" (prop_get_set EE.fullName)
            , testProperty "Law: set-set" (prop_set_set EE.fullName)
            ]
        ]



-- Properties


prop_set_get :: Lens' EE.User String -> Property
prop_set_get lens =
    property $ do
        structure <- forAll genUser
        newValue <- forAll genStringWithSpaceInIt
        view lens (set lens newValue structure) === newValue


prop_get_set :: Lens' EE.User String -> Property
prop_get_set lens =
    property $ do
        structure <- forAll genUser
        set lens (view lens structure) structure === structure


prop_set_set :: Lens' EE.User String -> Property
prop_set_set lens =
    property $ do
        structure <- forAll genUser
        newValue <- forAll genStringWithSpaceInIt
        differentValue <- forAll genStringWithSpaceInIt
        set lens differentValue (set lens newValue structure) === set lens differentValue structure



-- Generators


genString :: MonadGen m => m String
genString =
    Gen.list (Range.linear 1 50) Gen.alpha


genStringWithSpaceInIt :: MonadGen m => m String
genStringWithSpaceInIt =
    liftA2 (++)
        genString
        (fmap (" " ++) genString)


genUser :: MonadGen m => m EE.User
genUser =
    EE.User <$> genString <*> genString <*> genString
