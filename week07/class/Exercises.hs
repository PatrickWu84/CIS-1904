{-# LANGUAGE InstanceSigs #-}

module Exercises where

data Weather
  = Sunny
  | Cloudy

------

data Foo
  = A Int
  | B Weather

------

data Tree
  = Leaf
  | Branch Tree Int Tree

-- Exercise:
-- Write an Eq instance for Tree.
instance Eq Tree where
  (==) :: Tree -> Tree -> Bool
  Leaf == Leaf = True
  Branch l1 i1 r1 == Branch l2 i2 r2 = l1 == l2 && i1 == i2 && r1 == r2
  _ == _ = False

------

-- Exercise:
-- Implement a function removeAll, where
-- removeAll x xs removes all instances of x from xs.
removeAll :: (Eq a) => a -> [a] -> [a]
removeAll x = filter (/= x)

------

-- number of minutes, number of seconds
data Duration = Duration Int Int

-- Exercise:
-- Write an Eq and Show instance for Duration.
instance Eq Duration where
  (==) :: Duration -> Duration -> Bool
  Duration m1 s1 == Duration m2 s2 = m1 == m2 && s1 == s2

instance Show Duration where
  show :: Duration -> String
  show (Duration m s) = show m ++ "m " ++ show s ++ "s"