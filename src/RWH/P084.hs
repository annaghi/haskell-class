{-# LANGUAGE ScopedTypeVariables #-}

module RWH.P084
    ( safeHead
    , safeTail
    , safeInit
    , safeLast
    , splitWith
    ) where


safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs


safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)


splitWith :: forall a. (a -> Bool) -> [a] -> [[a]]
splitWith p =
    go
    where
        go :: [a] -> [[a]]
        go [] = []
        go xs =
            let
                ((prefix, suffix) :: ([a], [a])) = break p xs
            in
            case (prefix, suffix) of
                (x, []) -> 
                    [x]

                ([], (_:ys)) -> 
                    go ys

                (x, (y:ys)) ->
                    x : if p y then go ys else go (y:ys)
    