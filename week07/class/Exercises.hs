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

------

-- Exercise:
-- Implement a function removeAll, where
-- removeAll x xs removes all instances of x from xs.

------

-- number of minutes, number of seconds
data Duration = Duration Int Int

-- Exercise:
-- Write an Eq and Show instance for Duration.
