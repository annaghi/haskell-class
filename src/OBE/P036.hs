{-# LANGUAGE TemplateHaskell #-}

module OBE.P036
    ( Builder(..)
    , builder
    , context
    , build
    , destroy
    ) where

import Control.Lens (Lens', lens)
import Control.Lens.Combinators (makeLenses)


data Builder = Builder
    { _context :: [String]
    , _build :: [String] -> String
    , _destroy :: String -> [String]
    }
makeLenses ''Builder

instance Show Builder
    where
        show (Builder cs b _destroy) =
            b cs

instance Eq Builder
    where
        Builder cs1 b1 d1 == Builder cs2 b2 d2 =
            cs1 == cs2 && b1 cs1 == b2 cs2 && d1 (b1 cs1) == d2 (b2 cs2)


builder :: Lens' Builder String
builder =
    lens
        (\(Builder cs b _) -> b cs)
        (\(Builder _ b d) newValue -> Builder (d newValue) b d)
