{-# LANGUAGE ScopedTypeVariables #-}

module RWH.P070Test (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Utils ((|>))

import Data.List (nub, sort)
import Data.Tuple (swap)
import Control.Applicative (liftA2)
import GHC.Exts (sortWith)

import qualified RWH.P070 as EE


tests :: TestTree
tests =
    testGroup "Page 70"
        [ testGroup "Exercise 6: sortByLength"
            [ testCase "when the list is empty" $
                (null $ EE.sortByLength [])
                    |> assertEqual "" True
            , testCase "when the list has one item" $
                EE.sortByLength [[1,2,3]]
                    |> assertEqual "" [[1,2,3]]
            , testCase "when the list has more items" $
                EE.sortByLength [[1,2,3],[],[8],[2]]
                    |> assertEqual "" [[],[8],[2],[1,2,3]]
            , testProperty "mapped to length should result as the same as sorting lengths" $
                property $ do
                    xs <- forAll $ Gen.list (Range.linear 0 10) genString
                    (map length . EE.sortByLength) xs === (sort . map length) xs
            ]
        , testGroup "Exercise 7-8: intersperse"
            [ testCase "when the list is empty" $
                (null $ EE.intersperse ',' [])
                    |> assertEqual "" True
            , testCase "when the list has one item" $
                EE.intersperse ',' ["one"]
                    |> assertEqual "" "one"
            , testCase "when the list has more items" $
                EE.intersperse ',' ["one","two"]
                    |> assertEqual "" "one,two"
            , testProperty "with length should result as the same as the sum of the length of concatenation and the number of strings - 1" $
                property $ do
                    xs <- forAll $ Gen.list (Range.linear 1 10) genString
                    length (EE.intersperse ',' xs) === length (concat xs) + length xs - 1
            ]
        , testGroup "Exercise 9: height"
            [ testCase "when the tree is empty" $
                EE.height EE.Empty
                    |> assertEqual "" (-1)
            , testCase "when the tree has one node" $
                EE.height (EE.Node 3 EE.Empty EE.Empty)
                    |> assertEqual "" 0
            , testCase "when the tree has two nodes" $
                EE.height (EE.Node 'a' (EE.Node 'b' EE.Empty EE.Empty) EE.Empty)
                    |> assertEqual "" 1
            , testProperty "should result between (floor (log2 n)) and (n-1)" $
                property $ do
                    tree <- forAll genTree
                    let
                        count = EE.count tree
                    Hedgehog.assert $ EE.height tree <= count - 1
                    Hedgehog.assert $ EE.height tree >= floor (logBase 2 count)
            ]
        , testGroup "Exercise 10-11: direction"
            [ testCase "when turning left" $
                EE.turn (8,2) (4,5) (1,1)
                    |> assertEqual "" EE.Left
            , testCase "when turning right" $
                EE.turn (1,1) (4,5) (8,2)
                    |> assertEqual "" EE.Right
            , testCase "when going straight forward" $
                EE.turn (8,2) (4,5) (0,8)
                    |> assertEqual "" EE.Straight
            , testCase "when going straight backward" $
                EE.turn (8,2) (4,5) (8,2)
                    |> assertEqual "" EE.Straight
            , testCase "when going nowhere" $
                EE.turn (0,0) (0,0) (0,0)
                    |> assertEqual "" EE.Straight
            , testProperty "should result as the same as the opposite direction of reversed order" $
                property $ do
                    a <- forAll genPoint
                    b <- forAll genPoint
                    c <- forAll genPoint
                    EE.turn a b c === (EE.oppositeDirection $ EE.turn c b a)
            ]
        , testGroup "Exercise 12: turns"
            [ testCase "when turning left than right" $
                EE.turns [(0,0), (1,0), (0,1), (1,1)]
                    |> assertEqual "" [EE.Left, EE.Right]
            , testCase "when turning left than left" $
                EE.turns [(0,0), (1,0), (0,1), (0,0)]
                    |> assertEqual "" [EE.Left, EE.Left]
            , testProperty "should result as the same as the opposite direction of reflected through X axis" $
                property $ do
                    points <- forAll genPoints
                    EE.turns points === (map EE.oppositeDirection $ EE.turns $ EE.reflection1 points)
            ]
        , testGroup "Exercise 13: convex hull"
            [ testCase "sortByAngleWithP0" $
                EE.sortByAngleWithP0 (0,0) [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (3,1), (4,3)]
                    |> assertEqual "" [(0,0), (3,0), (4,1), (3,1), (2,1), (3,2), (4,3), (3,3), (2,2), (1,2)]
            , testCase "main algorithm" $
                EE.convexHull [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (0,0), (3,1), (4,3)]
                    |> assertEqual "" [(1,2), (3,3), (4,3), (4,1), (3,0), (0,0)]
            , testCase "inverse algorithm" $
                EE.convexHull' [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (0,0), (3,1), (4,3)]
                    |> assertEqual "" [(3,3), (1,2), (0,0), (3,0), (4,1), (4,3)]
            , testCase "should result as the same as the transformed inverse algorithm" $
                EE.convexHull [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (0,0), (3,1), (4,3)]
                    |> assertEqual "" (transform $ EE.convexHull' [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (0,0), (3,1), (4,3)])
            -- , testProperty "should result as the same as the transformed inverse algorithm - failing" $
            --     property $ do
            --         points <- forAll genPoints
            --         EE.convexHull points === (reverse $ EE.convexHull' points)
            ]
        ]



-- Generators


genString :: MonadGen m => m String
genString =
    Gen.list (Range.linear 0 4) Gen.alpha


genTree :: MonadGen m => m (EE.Tree String)
genTree =
    Gen.recursive Gen.choice
        [ pure $ EE.Node "v" EE.Empty EE.Empty ]
        [ EE.Node "v" EE.Empty <$> genTree
        , Gen.subtermM genTree (\tree -> pure $ EE.Node "v" tree EE.Empty)
        , EE.Node "v" <$> genTree <*> genTree
        ]


genPoint :: (MonadGen m, RealFloat a) => m (EE.Point a)
genPoint =
    liftA2 (,)
        (fmap fromInteger $ Gen.integral (Range.linearFrom 0 (-10) 10))
        (fmap fromInteger $ Gen.integral (Range.linearFrom 0 (-10) 10))
    -- (,)
    --     <$> (Gen.integral (Range.linearFrom 0 (-10) 10))
    --     <*> (Gen.integral (Range.linearFrom 0 (-10) 10))


genPoints :: (MonadGen m, RealFloat a, Ord a, Eq a) => m [EE.Point a]
genPoints =
    fmap nub $ Gen.list (Range.linear 0 10) genPoint



-- Helpers


transform :: forall a. (RealFloat a) => [EE.Point a] -> [EE.Point a]
transform points =
    reverse $ (\ps -> dropWhile (/= p0) ps ++ takeWhile (/= p0) ps) points
    where
        p0 :: EE.Point a
        p0 = head $ sortWith swap points
