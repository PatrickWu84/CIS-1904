-- Uncomment this if you're having trouble with type signatures in instances.
{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Data.Bifoldable (biList, bifoldl1)

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

{-
Exercise:
Write a Functor instance for Tree.
-}
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

{-
Exercise:
Use fmap to implement a function that adds three to every
element in the tree. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become Branch (Branch Leaf 4 Leaf) 5 (Branch Leaf 6 Leaf)

Try to make it work for more than just Int!
-}
add3Tree :: (Num a) => Tree a -> Tree a
add3Tree = fmap (+ 3)

{-
Exercise:
Write a Foldable instance for Tree.
-}
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z Leaf = z
  foldr f z (Branch l x r) = foldr f (f x (foldr f z r)) l

{-
Exercise:
Use foldr to implement a function that flattens a
tree into a list. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become [1, 2, 3]
-}
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []