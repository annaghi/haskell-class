import Test.Tasty
import Test.Tasty.HUnit
import Utils ((|>))

import RWH.P070


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [tests]


tests :: TestTree
tests =
    testGroup "RWH"
        [ testGroup "Page 070"
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
                ]
            ]
        ]
