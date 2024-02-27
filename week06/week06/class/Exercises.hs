module Exercises where

-- or

(||!) :: Bool -> Bool -> Bool
True ||! True = True
True ||! False = True
False ||! True = True
False ||! False = False

-- Fibonacci numbers

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Copy down the fibs definition locally!

-----------

-- streams

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamTake :: Int -> Stream a -> [a]
streamTake n = take n . streamToList

-- exercise:

-- First, implement this function.
-- streamIterate f a should return a, f a, f (f a), ...

streamIterate :: (a -> a) -> a -> Stream a
streamIterate = error "undefined"

-- Then, reimplement natural numbers as a stream.

nats :: Stream Int
nats = error "undefined"

-- You can test your code by running this in GHCi:

upTo10 :: [Int]
upTo10 = streamTake 10 nats