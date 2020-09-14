module RWH.P060
    ( toList
    , fromList
    , countNodes
    , test
    ) where


data List a
    = Cons a (List a)
    | Nil
    deriving (Show)


fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil


toList :: List a -> [a]
toList (Cons x xs) = x : toList xs
toList Nil = []


data Tree a
    = Node a (Maybe (Tree a)) (Maybe (Tree a))
    deriving (Show)


countNodes :: Tree a -> Int
countNodes (Node _ (Just left) (Just right)) = 1 + countNodes left + countNodes right
countNodes (Node _ Nothing (Just right))     = 1 + countNodes right
countNodes (Node _ (Just left) Nothing)      = 1 + countNodes left
countNodes (Node _ Nothing Nothing)          = 1



-- TESTS


test :: Bool
test = and
    [ toList Nil                   == ([] :: [()])
    , toList (Cons 1 Nil)          == ([1] :: [Int])
    , toList (Cons 2 (Cons 1 Nil)) == ([2,1] :: [Int])
    --
    , 1 == countNodes (Node "root" Nothing Nothing)
    , 3 == countNodes (Node "parent" (Just (Node "left child" Nothing Nothing)) (Just (Node "right child" Nothing Nothing)))
    ]
