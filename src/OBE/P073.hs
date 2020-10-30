{-# LANGUAGE TemplateHaskell #-}

module OBE.P073
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
    , goalA_Fn
    , goalB_Fn
    , goalC_Fn
    , goalA_Op
    , goalB_Op
    , goalC_Op
    , goalC_Op_State
    ) where

import Optics.TH (makeLenses)
import Optics.Getter (view)
import Optics.Setter (set, over)
import Optics.Optic ((%), (&))
import Optics.Operators ((.~), (%~), (^.))
import Optics.State.Operators ((<<%=))
import Control.Monad.State (runState)
import Data.Tuple.Optics (_1, _2)


data Army = Army
    { _archers :: Int
    , _knights :: Int
    }
    deriving (Show, Eq)

makeLenses ''Army


data Gate = Gate
    { _open :: Bool
    , _oilTemp :: Float
    }
    deriving (Show, Eq)

makeLenses ''Gate


data Kingdom = Kingdom
    { _name :: String
    , _army :: Army
    , _gate :: Gate
    }
    deriving (Show, Eq)

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


goalA_Fn :: Kingdom -> Kingdom
goalA_Fn =
    over name (++ ": a perfect place")
    . set (army % knights) 42
    . over (gate % open) not


goalB_Fn :: Kingdom -> Kingdom
goalB_Fn =
    set name "Dulocinstein"
    . set (army % archers) 17
    . set (army % knights) 26
    . over (gate % oilTemp) (* 10)


goalC_Fn :: Kingdom -> (String, Kingdom)
goalC_Fn kingdom =
    ( view name $ over name (++ ": Home") kingdom
    , goalC'_Fn kingdom
    )
    where
        goalC'_Fn :: Kingdom -> Kingdom
        goalC'_Fn =
            over name (++ ": Home of the talking Donkeys")
            . over (gate % oilTemp) (/ 2)


goalA_Op :: Kingdom -> Kingdom
goalA_Op kingdom =
    kingdom
    & name %~ (++ ": a perfect place")
    & army % knights .~ 42
    & gate % open %~ not


goalB_Op :: Kingdom -> Kingdom
goalB_Op kingdom =
    kingdom
    & name .~ "Dulocinstein"
    & army % archers .~ 17
    & army % knights .~ 26
    & gate % oilTemp %~ (* 10)


goalC_Op :: Kingdom -> (String, Kingdom)
goalC_Op kingdom =
    ( (kingdom & name %~ (++ ": Home")) ^. name
    , goalC'_Op
    )
    where
        goalC'_Op :: Kingdom
        goalC'_Op =
            kingdom
            & name %~ (++ ": Home of the talking Donkeys")
            & gate % oilTemp %~ (/ 2)


goalC_Op_State :: Kingdom -> (String, Kingdom)
goalC_Op_State kingdom =
    kingdom
    & runState ((<<%=) name (++ ": Home of the talking Donkeys"))
    & _1 %~ (++ ": Home")
    & _2 % gate % oilTemp %~ (/ 2)
