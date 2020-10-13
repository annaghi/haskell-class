import Test.Tasty

import RWH.Test
import HW03.Test
import HW04.Test


main :: IO ()
main = defaultMain $ testGroup "Tests" $
    [ RWH.Test.suite
    , testGroup "CIS-194"
        [ HW03.Test.suite
        , HW04.Test.suite
        ]
    ]
