import Test.Tasty

import RWH.P070Test
import HW03.Test
import HW04.Test


main :: IO ()
main = defaultMain $ testGroup "Tests" $
    [ testGroup "RWH"
        [ RWH.P070Test.tests ]
    , testGroup "CIS-194"
        [ HW03.Test.suite
        , HW04.Test.suite
        ]
    ]
