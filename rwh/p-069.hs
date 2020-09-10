{-# OPTIONS_GHC -Wall #-}

module Page60
    ( length'
    , mean
    , test
    ) where


length' :: Num b => [a] -> b
length' []     = 0
length' (_:xs) = 1 + length' xs


mean :: (Fractional a) => [a] -> Maybe a
mean [] = Nothing
mean xs = Just (sum xs / length'' xs)
    where
        length'' :: Num a => [a] -> a
        length'' = fromIntegral . length


-- TESTS

test :: Bool
test = and
    [ length' []      == length []
    , length' "abcde" == length "abcde"
    , mean []         == (Nothing :: Maybe Double)
    , mean [1,2,3,4]  == (Just 2.5 :: Maybe Double)
    ]
