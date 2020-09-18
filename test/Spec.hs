import Test.Tasty

import RWH.P070Test
import HW04.E1Test


main :: IO ()
main = defaultMain $ testGroup "Tests" $
    [ testGroup "RWH"
        [ RWH.P070Test.tests ]
    , testGroup "HW04"
        [ HW04.E1Test.tests ]
    ]
