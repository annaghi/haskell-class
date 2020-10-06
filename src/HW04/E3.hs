{-# LANGUAGE ScopedTypeVariables #-}

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


{-
https://www.cs.nott.ac.uk/~pszgmh/fold.pdf

myFoldl :: forall a b. (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs =
    myFoldl' f xs base
    where
        myFoldl' :: (a -> b -> a) -> [b] -> a -> a
        myFoldl' f [] base = base
        myFoldl' f (x:xs) base = myFoldl' f xs (f base x)

myFoldl' f [] = v 
myFoldl' f (x:xs) = g x (myFoldl' f xs)    ⟺   myFoldl' = foldr g v

myFoldl' f [] = \base -> base
myFoldl' f (x:xs) = g x (myFoldl' f xs)
⟺ myFoldl' f (x:xs) base = g x (myFoldl' f xs) base
⟺ myFoldl' f xs (f base x) = g x (myFoldl' f xs) base (h := myFoldl' f xs)
⟺ h (f base x) = g x h base
⟹ g = \x h -> (\base -> h (f base x))
⟹ myFoldl' = foldr (\x h -> (\base -> h (f base x))) (\base -> base)
-}


myFoldl :: forall a b. (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs =
    -- foldr (\x h -> (\base' -> h (f base' x))) id xs base
    foldr step id xs base
    where
        step :: b -> (a -> a) -> a -> a
        step x h base' =
            h (f base' x)


{- 
Highlight the fact that foldr reduces to function of type a -> a:

myFoldl :: forall a. (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = go base
    where
        go :: a -> a
        go = foldr (\x h -> (\base' -> h (f base' x))) (\base' -> base') xs
-}
