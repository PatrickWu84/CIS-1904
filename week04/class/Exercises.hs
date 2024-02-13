module Exercises where

import Data.List (sort)
import Prelude hiding (and, sum)

-- #1 Use function names directly

absAll :: [Int] -> [Int]
absAll xs = map (\x -> abs x) xs

absAll' :: [Int] -> [Int]
absAll' xs = map abs xs

-- ...when using infix operators

sum :: [Int] -> Int
sum xs = foldr (\x acc -> x + acc) 0 xs

sum' :: [Int] -> Int
sum' xs = foldr (+) 0 xs

and :: [Bool] -> Bool
and xs = foldr (\x acc -> x && acc) True xs

and' :: [Bool] -> Bool
and' xs = foldr (\x acc -> x && acc) True xs -- try this one

--------------

-- #2 Leverage function composition

desort :: [Int] -> [Int]
desort xs = reverse (sort xs)

desort' :: [Int] -> [Int]
desort' = reverse . sort

-- ...when constructing arguments to other functions

oddOnly :: [Int] -> [Int]
oddOnly xs = filter (\x -> not (even x)) xs

oddOnly' :: [Int] -> [Int]
oddOnly' xs = filter (not . even) xs

-- ...when combining more than two functions

evenOdds :: [Int] -> Bool
evenOdds xs = even (length (oddOnly xs))

evenOdds' :: [Int] -> Bool
evenOdds' xs = even (length (oddOnly xs)) -- try this one

--------------

-- #3 Leverage partial application.

-- ...by eta reducing

absAll'' :: [Int] -> [Int]
absAll'' = map abs

and'' :: [Bool] -> Bool
and'' xs = foldr (&&) True xs -- try this one

-- ...by using operator sections

add3All :: [Int] -> [Int]
add3All = map (\x -> 3 + x)

add3All' :: [Int] -> [Int]
add3All' = map (3 +)

greaterThan100 :: [Int] -> [Int]
greaterThan100 = filter (\x -> x > 100)

greaterThan100' :: [Int] -> [Int]
greaterThan100' = filter (\x -> x > 100) -- try this one

--------------

-- All together now!

-- example from last class:

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs -- follow along locally

-- another example from last class:

youngNames :: [Person] -> [String]
youngNames xs = map name (filter (\x -> age x <= 18) xs)

youngNames' :: [Person] -> [String]
youngNames' xs = map name (filter (\x -> age x <= 18) xs) -- follow along locally

data Person = Person String Int
  deriving (Show)

name :: Person -> String
name (Person s _) = s

age :: Person -> Int
age (Person _ n) = n
