{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Exercises where

import BST
import Test.QuickCheck

{- Read [instructions.md] first. -}

-- Exercise 1

instance Arbitrary Tree where
  arbitrary :: Gen Tree
  arbitrary = sized (\n -> genTree (-n) n)
    where
      genTree :: Int -> Int -> Gen Tree
      genTree lower upper
        | lower > upper = return Leaf
        | otherwise =
            frequency
              [ (1, return Leaf),
                ( 3,
                  do
                    x <- choose (lower, upper)
                    l <- genTree lower (x - 1)
                    r <- genTree (x + 1) upper
                    return (Branch l x r)
                )
              ]

prop_ArbitraryValid :: Tree -> Bool
prop_ArbitraryValid = isBST

-- Exercise 2

prop_FindPostPresent :: Int -> Tree -> Bool
prop_FindPostPresent x t = find x (insert x t)

prop_FindPostAbsent :: Int -> Tree -> Bool
prop_FindPostAbsent x t = not (find x (delete x t))

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = 1

question :: String
question = "What are the downsides of property based testing?"
