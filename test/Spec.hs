import Test.Tasty

import RWH.P070Test
import RWH.P084Test
import RWH.P097Test
import RWH.P098Test
import HW03.Test
import HW04.Test


main :: IO ()
main = defaultMain $ testGroup "Tests" $
    [ testGroup "RWH"
        [ RWH.P070Test.tests
        , RWH.P084Test.tests
        , RWH.P097Test.tests
        , RWH.P098Test.tests
        ]
    , testGroup "CIS-194"
        [ HW03.Test.suite
        , HW04.Test.suite
        ]
    ]
