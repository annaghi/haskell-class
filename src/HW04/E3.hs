{-# LANGUAGE ScopedTypeVariables #-}

module HW04.E3
    ( xor
    , map'
    , myFoldl
    , myFoldl1
    , myFoldl2
    , myFoldl3
    ) where


xor :: [Bool] -> Bool
xor = 
    foldr (\e a -> if e then not a else a) False


map' :: (a -> b) -> [a] -> [b]
map' f =
    foldr ((:) . f) []



-- https://www.cs.nott.ac.uk/~pszgmh/fold.pdf

myFoldl1 :: forall a b. (a -> b -> a) -> a -> [b] -> a
myFoldl1 step base xs =
    myFoldl' step xs base
    where
        myFoldl' :: (a -> b -> a) -> [b] -> a -> a
        myFoldl' _ [] base' = base'
        myFoldl' f (y:ys) base' = myFoldl' f ys (f base' y)


{-
myFoldl' f [] = v 
myFoldl' f (y:ys) = g y (myFoldl' f ys)    ⟺   myFoldl' = foldr g v

myFoldl' f [] = v
⟹ v = \base' -> base'

myFoldl' f (y:ys) = g y (myFoldl' f ys)
⟺ myFoldl' f (y:ys) base' = g y (myFoldl' f ys) base'
⟺ myFoldl' f ys (f base' y) = g y (myFoldl' f ys) base'     // h := myFoldl' f ys
⟺ h (f base' y) = g y h base'
⟹ g = \y h base' -> h (f base' y)
⟹ myFoldl' = foldr (\y h base' -> h (f base' y)) (\base' -> base')
-}
myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f base xs =
    foldr (\y h base' -> h (f base' y)) id xs base


--Highlight the fact that foldr reduces to function of type a -> a:
myFoldl3 :: forall a b. (a -> b -> a) -> a -> [b] -> a
myFoldl3 f base xs =
    go base
    where
        go :: a -> a
        go = foldr (\x h base' -> h (f base' x)) (\base' -> base') xs


myFoldl :: forall a b. (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs =
    foldr step id xs base
    where
        step :: b -> (a -> a) -> a -> a
        step x h base' =
            h (f base' x)
