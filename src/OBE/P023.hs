module OBE.P023
    ( Inventory(..)
    , wand
    , book
    , potions
    ) where

import Control.Lens (Lens', lens)


type Wand =
    String


type Book =
    String


type Potion =
    String


data Inventory = Inventory
    { _wand :: Wand
    , _book :: Book
    , _potions :: [Potion]
    }
    deriving (Show)


wand :: Lens' Inventory Wand
wand =
    lens getWand setWand
    where
        getWand :: Inventory -> Wand
        getWand =
            _wand

        setWand :: Inventory -> Wand -> Inventory
        setWand inventory newWand =
            inventory { _wand = newWand }


book :: Lens' Inventory Book
book =
    lens getBook setBook
    where
        getBook :: Inventory -> Book
        getBook =
            _book

        setBook :: Inventory -> Book -> Inventory
        setBook inventory newBook =
            inventory { _book = newBook }


potions :: Lens' Inventory [Potion]
potions =
    lens getPotions setPotions
    where
        getPotions :: Inventory -> [Potion]
        getPotions =
            _potions

        setPotions :: Inventory -> [Potion] -> Inventory
        setPotions inventory newPotions =
            inventory { _potions = newPotions }
