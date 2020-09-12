module Utils
    ( (|>), (<|), (>>)
    ) where


import Prelude hiding ((>>))


{-| Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
-}
(|>) :: a -> (a -> b) -> b
x |> f = f x


{-| Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis.
-}
(<|) :: (a -> b) -> a -> b
f <| x = f x


infixr 1 <|
infixl 1 |>


(>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >> g = \x -> f x |> g
