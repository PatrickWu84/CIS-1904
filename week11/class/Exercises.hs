module Exercises where

import Test.QuickCheck
import Data.Bool (Bool)

-- Source: https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
sort :: [Int] -> [Int]
sort [] = []
sort (p : xs) = sort lesser ++ [p] ++ sort greater
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

-- Bad alternative #1.
-- sort :: [Int] -> [Int]
-- sort = id

-- Bad alternative #2.
-- sort :: [Int] -> [Int]
-- sort [] = []
-- sort (p : xs) = sort lesser ++ [p] ++ sort greater
--   where
--     lesser = filter (< p) xs
--     greater = filter (> p) xs

prop_SortOrdered :: [Int] -> Bool
prop_SortOrdered xs = ordered (sort xs)

ordered :: [Int] -> Bool
ordered [] = True
ordered [x] = True
ordered (x1 : x2 : xs) = x1 <= x2 && ordered (x2 : xs)

-- Exercise: Write a property that catches the bug in #2.
prop_SortEqual :: [Int] -> Bool
prop_SortEqual xs ys = length xs == length ys

--------

data Expr
  = Num Int
  | Add Expr Expr
  deriving (Show)

-- Exercise: Write an Arbitrary instance for Expr.
instance Arbitrary a => Arbitrary [a] where
  arbitrary :: Gen [a]
  arbitrary = 
    frequency
      [ (1, return [])]
--------

remove :: Int -> [Int] -> [Int]
remove n [] = []
remove n (m : ms)
  | n < m = m : ms
  | n == m = rest
  | otherwise = m : rest
  where
    rest = remove n ms