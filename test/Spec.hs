import Test.Tasty

import RWH.P070Test


main :: IO ()
main = defaultMain $ testGroup "Tests" $
    [ testGroup "RWH"
        [ RWH.P070Test.tests ]
    ]
