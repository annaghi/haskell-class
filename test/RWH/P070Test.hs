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

import qualified RWH.P070 as XX


tests :: TestTree
tests =
    testGroup "Page 070"
        [ testGroup "Exercise 06: sortByLength"
            [ testCase "when the list is empty" $
                (null $ XX.sortByLength [])
                    |> assertEqual "" True
            , testCase "when the list has one item" $
                XX.sortByLength [[1,2,3]]
                    |> assertEqual "" [[1,2,3]]
            , testCase "when the list has more items" $
                XX.sortByLength [[1,2,3],[],[8],[2]]
                    |> assertEqual "" [[],[8],[2],[1,2,3]]
            , testProperty "mapped to length should result as the same as sorting lengths" $
                property $ do
                    xs <- forAll $ Gen.list (Range.linear 0 10) genString
                    (map length . XX.sortByLength) xs === (sort . map length) xs
            ]
        , testGroup "Exercise 07-08: intersperse"
            [ testCase "when the list is empty" $
                (null $ XX.intersperse ',' [])
                    |> assertEqual "" True
            , testCase "when the list has one item" $
                XX.intersperse ',' ["one"]
                    |> assertEqual "" "one"
            , testCase "when the list has more items" $
                XX.intersperse ',' ["one","two"]
                    |> assertEqual "" "one,two"
            , testProperty "with length should result as the same as the sum of the length of concatenation and the number of strings - 1" $
                property $ do
                    xs <- forAll $ Gen.list (Range.linear 1 10) genString
                    length (XX.intersperse ',' xs) === length (concat xs) + length xs - 1
            ]
        , testGroup "Exercise 09: height"
            [ testCase "when the tree is empty" $
                XX.height XX.Empty
                    |> assertEqual "" (-1)
            , testCase "when the tree has one node" $
                XX.height (XX.Node 3 XX.Empty XX.Empty)
                    |> assertEqual "" 0
            , testCase "when the tree has two nodes" $
                XX.height (XX.Node 'a' (XX.Node 'b' XX.Empty XX.Empty) XX.Empty)
                    |> assertEqual "" 1
            , testProperty "should result between (floor (log2 n)) and (n-1)" $
                property $ do
                    tree <- forAll genTree
                    let
                        count = XX.count tree
                    Hedgehog.assert $ XX.height tree <= count - 1
                    Hedgehog.assert $ XX.height tree >= floor (logBase 2 count)
            ]
        , testGroup "Exercise 10-11: direction"
            [ testCase "when turning left" $
                XX.turn (8,2) (4,5) (1,1)
                    |> assertEqual "" XX.Left
            , testCase "when turning right" $
                XX.turn (1,1) (4,5) (8,2)
                    |> assertEqual "" XX.Right
            , testCase "when going straight forward" $
                XX.turn (8,2) (4,5) (0,8)
                    |> assertEqual "" XX.Straight
            , testCase "when going straight backward" $
                XX.turn (8,2) (4,5) (8,2)
                    |> assertEqual "" XX.Straight
            , testCase "when going nowhere" $
                XX.turn (0,0) (0,0) (0,0)
                    |> assertEqual "" XX.Straight
            , testProperty "should result as the same as the opposite direction of reversed order" $
                property $ do
                    a <- forAll genPoint
                    b <- forAll genPoint
                    c <- forAll genPoint
                    XX.turn a b c === (XX.oppositeDirection $ XX.turn c b a)
            ]
        , testGroup "Exercise 12: turns"
            [ testCase "when turning left than right" $
                XX.turns [(0,0), (1,0), (0,1), (1,1)]
                    |> assertEqual "" [XX.Left, XX.Right]
            , testCase "when turning left than left" $
                XX.turns [(0,0), (1,0), (0,1), (0,0)]
                    |> assertEqual "" [XX.Left, XX.Left]
            , testProperty "should result as the same as the opposite direction of reflected through X axis" $
                property $ do
                    points <- forAll genPoints
                    XX.turns points === (map XX.oppositeDirection $ XX.turns $ XX.reflection1 points)
            ]
        , testGroup "Exercise 13: convex hull"
            [ testCase "sortByAngleWithP0" $
                XX.sortByAngleWithP0 (0,0) [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (3,1), (4,3)]
                    |> assertEqual "" [(0,0), (3,0), (4,1), (3,1), (2,1), (3,2), (4,3), (3,3), (2,2), (1,2)]
            , testCase "main algorithm" $
                XX.convexHull [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (0,0), (3,1), (4,3)]
                    |> assertEqual "" [(1,2), (3,3), (4,3), (4,1), (3,0), (0,0)]
            , testCase "inverse algorithm" $
                XX.convexHull' [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (0,0), (3,1), (4,3)]
                    |> assertEqual "" [(3,3), (1,2), (0,0), (3,0), (4,1), (4,3)]
            , testCase "should result as the same as the transformed inverse algorithm" $
                XX.convexHull [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (0,0), (3,1), (4,3)]
                    |> assertEqual "" (transform $ XX.convexHull' [(2,2), (3,2), (3,0), (4,1), (2,1), (3,3), (1,2), (0,0), (3,1), (4,3)])
            , testProperty "should result as the same as the transformed inverse algorithm - failing" $
                property $ do
                    points <- forAll genPoints
                    XX.convexHull points === (reverse $ XX.convexHull' points)
            ]
        ]



-- Generators


genString :: MonadGen m => m String
genString =
    Gen.list (Range.linear 0 4) Gen.alpha


genTree :: MonadGen m => m (XX.Tree String)
genTree =
    Gen.recursive Gen.choice
        [ pure $ XX.Node "v" XX.Empty XX.Empty ]
        [ XX.Node "v" XX.Empty <$> genTree
        , Gen.subtermM genTree (\tree -> pure $ XX.Node "v" tree XX.Empty)
        , XX.Node "v" <$> genTree <*> genTree
        ]


genPoint :: (MonadGen m, RealFloat a) => m (XX.Point a)
genPoint =
    liftA2 (,)
        (fmap fromInteger $ Gen.integral (Range.linearFrom 0 (-10) 10))
        (fmap fromInteger $ Gen.integral (Range.linearFrom 0 (-10) 10))
    -- (,)
    --     <$> (Gen.integral (Range.linearFrom 0 (-10) 10))
    --     <*> (Gen.integral (Range.linearFrom 0 (-10) 10))


genPoints :: (MonadGen m, RealFloat a, Ord a, Eq a) => m [XX.Point a]
genPoints =
    fmap nub $ Gen.list (Range.linear 0 10) genPoint



-- Helpers


transform :: forall a. (RealFloat a) => [XX.Point a] -> [XX.Point a]
transform points =
    reverse $ (\ps -> dropWhile (/= p0) ps ++ takeWhile (/= p0) ps) points
    where
        p0 :: XX.Point a
        p0 = head $ sortWith (swap) points
