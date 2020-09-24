{-# LANGUAGE ScopedTypeVariables #-}

module HW04.E2
    ( Tree(..)
    , foldTree
    , height
    , balanced
    ) where


 -- https://en.wikipedia.org/wiki/B-tree


data Tree a
    = Empty
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree = foldr insert Empty


insert :: forall a. a -> Tree a -> Tree a
insert x Empty = Node 0 Empty x Empty
insert x tree@(Node _ left _ right)
    | leftH  < rightH = insertLeft x tree
    | leftH  > rightH = insertRight x tree
    | leftH == rightH =
        let
            newRight :: Tree a
            newRight = insertRight x tree

            newLeft :: Tree a
            newLeft  = insertLeft x tree
        in
        if height newLeft <= height newRight then
            newLeft
        else
            newRight
    where
        leftH :: Integer
        leftH = height left

        rightH :: Integer
        rightH = height right
insert _ tree = tree


insertLeft :: forall a. a -> Tree a -> Tree a
insertLeft x Empty = Node 0 Empty x Empty
insertLeft x (Node _ left v right) =
    Node (1 + maxH newLeft right) newLeft v right
    where
        newLeft :: Tree a
        newLeft = insert x left


insertRight :: forall a. a -> Tree a -> Tree a
insertRight x Empty = Node 0 Empty x Empty
insertRight x (Node _ left v right) =
    Node (1 + maxH left newRight) left v newRight
    where
        newRight :: Tree a
        newRight = insert x right


height :: Tree a -> Integer
height Empty          = (-1)
height (Node h _ _ _) = h


maxH :: Tree a -> Tree a -> Integer
maxH t1 t2 =
    max (height t1) (height t2)


balanced :: Tree a -> Bool
balanced Empty = True
balanced t@(Node _ l _ r) =
    balanceFactor t <= 1
    && balanced l
    && balanced r


balanceFactor :: forall a. Tree a -> Integer
balanceFactor Empty = 0
balanceFactor (Node _ l _ r) =
    height' l - height' r
    where
        height' :: Tree a -> Integer
        height' Empty = (-1)
        height' (Node _ l' _ r') = 1 + max (height' l') (height' r')
