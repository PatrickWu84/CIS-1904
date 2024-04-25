{-# LANGUAGE TemplateHaskell #-}

import Data.Bool (Bool)
import Test.QuickCheck

sort :: [Int] -> [Int]
sort [] = []
sort (p : xs) = sort lesser ++ [p] ++ sort greater
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

prop_SortOrdered :: [Int] -> Bool
prop_SortOrdered xs = ordered (sort xs)

ordered :: [Int] -> Bool
ordered [] = True
ordered [x] = True
ordered (x1 : x2 : xs) = x1 <= x2 && ordered (x2 : xs)

prop_SortEqual :: [Int] -> Bool
prop_SortEqual xs ys = length xs == length ys

return []

main :: IO Bool
main = $quickCheckAll
