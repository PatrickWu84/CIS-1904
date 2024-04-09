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
        | lower > upper = error "unimplemented"
        | otherwise = error "unimplemented"

prop_ArbitraryValid :: Tree -> Bool
prop_ArbitraryValid = isBST

-- Exercise 2

prop_FindPostPresent :: Int -> Tree -> Bool
prop_FindPostPresent = error "unimplemented"

prop_FindPostAbsent :: Int -> Tree -> Bool
prop_FindPostAbsent = error "unimplemented"

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = error "unimplemented"

question :: String
question = error "unimplemented"
