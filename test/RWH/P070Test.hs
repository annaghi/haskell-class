module RWH.P070Test (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Utils ((|>))
import qualified Data.List as List

import RWH.P070


tests :: TestTree
tests =
    testGroup "Page 070"
        [ testGroup "Exercise 06: sortByLength"
            [ testCase "when the list is empty" $
                (null $ RWH.P070.sortByLength [])
                    |> assertEqual "" True
            , testCase "when the list has one item" $
                RWH.P070.sortByLength [[1,2,3]]
                    |> assertEqual "" [[1,2,3]]
            , testCase "when the list has more items" $
                RWH.P070.sortByLength [[1,2,3],[],[8],[2]]
                    |> assertEqual "" [[],[8],[2],[1,2,3]]
            , testProperty "mapped to length should result as the same as sorting lengths" $
                property $ do
                    xs <- forAll $ Gen.list (Range.linear 0 10) genString
                    (map length . RWH.P070.sortByLength) xs === (List.sort . map length) xs
            ]
        , testGroup "Exercise 07-08: intersperse"
            [ testCase "when the list is empty" $
                (null $ RWH.P070.intersperse ',' [])
                    |> assertEqual "" True
            , testCase "when the list has one item" $
                RWH.P070.intersperse ',' ["one"]
                    |> assertEqual "" "one"
            , testCase "when the list has more items" $
                RWH.P070.intersperse ',' ["one","two"]
                    |> assertEqual "" "one,two"
            , testProperty "length should result as the same as the sum of the length of concatenation and the number of strings - 1" $
                property $ do
                    xs <- forAll $ Gen.list (Range.linear 1 10) genString
                    length (RWH.P070.intersperse ',' xs) === length (concat xs) + length xs - 1
            ]
        , testGroup "Exercise 09: height"
            [ testCase "when the tree is empty" $
                RWH.P070.height RWH.P070.Empty
                    |> assertEqual "" (-1)
            , testCase "when the tree has one node" $
                RWH.P070.height (RWH.P070.Node 3 RWH.P070.Empty RWH.P070.Empty)
                    |> assertEqual "" 0
            , testCase "when the tree has two nodes" $
                RWH.P070.height (RWH.P070.Node 'a' (RWH.P070.Node 'b' RWH.P070.Empty RWH.P070.Empty) RWH.P070.Empty)
                    |> assertEqual "" 1
            , testProperty "should result between (floor (log2 n)) and (n-1)" $
                property $ do
                    tree <- forAll genTree
                    let
                        count = RWH.P070.count tree
                    Hedgehog.assert $ RWH.P070.height tree <= count - 1
                    Hedgehog.assert $ RWH.P070.height tree >= floor (logBase 2 count)
            ]
        ]



-- Generators


genString :: MonadGen m => m String
genString = Gen.list (Range.linear 0 4) Gen.alpha


genTree :: MonadGen m => m (RWH.P070.Tree String)
genTree =
    Gen.recursive Gen.choice
        [ pure $ RWH.P070.Node "v" RWH.P070.Empty RWH.P070.Empty ]
        [ RWH.P070.Node "v" RWH.P070.Empty <$> genTree
        , Gen.subtermM genTree (\tree -> pure $ RWH.P070.Node "v" tree RWH.P070.Empty)
        , RWH.P070.Node "v" <$> genTree <*> genTree
        ]
