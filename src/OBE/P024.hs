{-# LANGUAGE TemplateHaskell #-}

module OBE.P024
    ( Pet
    , petName
    , petType
    ) where

import Control.Lens.Combinators (makeLenses)


data Pet =
    Pet
        { _petName :: String
        , _petType :: String
        }
    deriving (Show)

makeLenses ''Pet
