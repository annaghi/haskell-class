module RWH.P084
    ( safeHead
    , safeTail
    , safeInit
    , safeLast
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
