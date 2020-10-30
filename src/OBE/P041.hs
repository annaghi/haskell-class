{-# LANGUAGE TemplateHaskell #-}

module OBE.P041
    ( User(..)
    , firstName
    , userName
    , fullName
    ) where

import Optics.Lens (Lens', lens)
import Optics.Getter (view)
import Optics.Setter (set)
import Optics.TH (makeLenses)
import Data.List.Extra (trim)


data User = User
    { _firstName :: String
    , _lastName :: String
    -- , _username :: String
    , _email :: String
    }
    deriving (Show, Eq)

makeLenses ''User


userName :: Lens' User String
userName =
    lens
        (view email)
        (\user newUsername -> set email newUsername user)


fullName :: Lens' User String
fullName =
    lens
        (\user -> view firstName user ++ " " ++ view lastName user)
        (\user newFullname ->
            let
                (firstN, lastN) =
                    span (/= ' ') newFullname
            in
            set firstName firstN
            $ set lastName (trim lastN)
            $ user
        )
