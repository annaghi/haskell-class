module HW04.E3
    ( xor
    , map'
    , myFoldl
    ) where


xor :: [Bool] -> Bool
xor = 
    foldr (\e a -> if e then not a else a) False


map' :: (a -> b) -> [a] -> [b]
map' f =
    foldr ((:) . f) []


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs =
    foldr (flip f) base $ reverse xs
