{-# LANGUAGE TemplateHaskell #-}

module OBE.P062
    ( Army(..)
    , Gate(..)
    , Kingdom(..)
    , name
    , army
    , gate
    , archers
    , knights
    , open
    , oilTemp
    , duloc
    , goalA
    , goalB
    , goalC
    ) where

import Optics.TH (makeLenses)
import Optics.Getter (view)
import Optics.Setter (set, over)
import Optics.Optic ((%))


-- Cummulative exercises from page 62 with types from page 73

data Army = Army
    { _archers :: Int
    , _knights :: Int
    } deriving (Show, Eq)
makeLenses ''Army


data Gate = Gate
    { _open :: Bool
    , _oilTemp :: Float
    } deriving (Show, Eq)
makeLenses ''Gate


data Kingdom = Kingdom
    { _name :: String
    , _army :: Army
    , _gate :: Gate
    } deriving (Show, Eq)
makeLenses ''Kingdom


duloc :: Kingdom
duloc =
    Kingdom
        { _name = "Duloc"
        , _army = Army
            { _archers = 22
            , _knights = 14
            }
        , _gate = Gate
            { _open = True
            , _oilTemp = 10.0
            }
        }


goalA :: Kingdom -> Kingdom
goalA =
    over name (++ ": a perfect place")
    . set (army % knights) 42
    . over (gate % open) not


goalB :: Kingdom -> Kingdom
goalB =
    set name "Dulocinstein"
    . set (army % archers) 17
    . set (army % knights) 26
    . over (gate % oilTemp) (* 10)


goalC :: Kingdom -> (String, Kingdom)
goalC kingdom =
    (view name $ over name (++ ": Home") kingdom, goalC' kingdom)


goalC' :: Kingdom -> Kingdom
goalC' =
    over name (++ ": Home of the talking Donkeys")
    . over (gate % oilTemp) (/ 2)
