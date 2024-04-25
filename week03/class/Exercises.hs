module Exercises where

-- Exercise 1:

-- Store a person's name and age.
data Person = Person String Int
  deriving (Show)

-- Fill in these two functions:
-- One to get a person's name, and one to get their age.

name :: Person -> String
name (Person s _) = s

age :: Person -> Int
age (Person _ n) = n

-- Return the names of all people in the input list age 18 or under.
-- Use map and filter.
youngNames :: [Person] -> [String]
youngNames xs = map name (filter (\x -> age x <= 18) xs)

-- youngNames peopleInput should return ["Bob", "Jill"].

peopleInput :: [Person]
peopleInput = [Person "Bob" 12, Person "Jack" 23, Person "Jill" 18, Person "Alice" 70]

-- Exercise 2:

-- Reimplement map and filter using foldr.

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs