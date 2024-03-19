-- Uncomment this if you're having trouble with type signatures in instances.
-- {-# LANGUAGE InstanceSigs #-}

module Exercises where

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

{-
Exercise:
Write a Functor instance for Tree.
-}

{-
Exercise:
Use fmap to implement a function that adds three to every
element in the tree. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become Branch (Branch Leaf 4 Leaf) 5 (Branch Leaf 6 Leaf)

Try to make it work for more than just Int!
-}

{-
Exercise:
Write a Foldable instance for Tree.
-}

{-
Exercise:
Use foldr to implement a function that flattens a
tree into a list. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become [1, 2, 3]
-}