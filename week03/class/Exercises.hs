module Exercises where

-- Exercise 1:

-- Store a person's name and age.
data Person = Person String Int
  deriving (Show)

-- Fill in these two functions:
-- One to get a person's name, and one to get their age.

-- name :: ???
-- name = error "unimplemented"

-- age :: ???
-- age = error "unimplemented"

-- Return the names of all people in the input list under age 18.
-- Use map and filter.
youngNames :: [Person] -> [String]
youngNames = error "unimplemented"

-- youngNames peopleInput should return ["Bob", "Jill"].

peopleInput :: [Person]
peopleInput = [Person "Bob" 12, Person "Jack" 23, Person "Jill" 18, Person "Alice" 70]

-- Exercise 2:

-- Reimplement map and filter using foldr.

map' :: (a -> b) -> [a] -> [b]
map' = error "unimplemented"

filter' :: (a -> Bool) -> [a] -> [a]
filter' = error "unimplemented"