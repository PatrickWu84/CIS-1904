module Exercises where

import Control.Concurrent (yield)
import Data.Bits (Bits (xor))
import Test.HUnit
  ( Test (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )

{- Read [instructions.md] first. -}

-- Exercise 1:

data Nat
  = Z
  | S Nat
  deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Z y = y
add (S x) y = add x (S y)

exercise1 :: Test
exercise1 =
  "add"
    ~: [ add zero zero ~?= zero,
         add one two ~?= three,
         add two one ~?= three
       ]

-- Exercise 2:

data Exp
  = Num Nat
  | Add Exp Exp
  | Branch Exp Exp Exp
  deriving (Show)

eval :: Exp -> Nat
eval (Num n) = n
eval (Add e1 e2) = add (eval e1) (eval e2)
eval (Branch e1 e2 e3)
  | eval e1 == Z = eval e2
  | otherwise = eval e3

exercise2 :: Test
exercise2 =
  "eval"
    ~: [ eval (Num three) ~?= three,
         eval (Add (Num one) (Add (Num two) (Num three))) ~?= six,
         eval (Branch (Num zero) (Num one) (Num two)) ~?= one,
         eval (Branch (Num one) (Num one) (Num two)) ~?= two
       ]

-- Exercise 3:

data Instr
  = IPush Nat
  | IAdd
  | IBranch
  deriving (Show)

type Stack = [Nat]

exec1 :: Instr -> Stack -> Stack
exec1 (IPush n) stack = n : stack
exec1 IAdd stack = case stack of
  (x : x2 : xs) -> add x x2 : xs
  _ -> stack
exec1 IBranch (x : x2 : x3 : xs)
  | x == Z = x2 : xs
  | otherwise = x3 : xs
exec1 IBranch x = x

exercise3 :: Test
exercise3 =
  "exec1"
    ~: [ exec1 (IPush one) [two] ~?= [one, two],
         exec1 IAdd [one, two, zero] ~?= [three, zero],
         exec1 IAdd [one] ~?= [one],
         exec1 IBranch [zero, one, two, three] ~?= [one, three],
         exec1 IBranch [one, one, two, three] ~?= [two, three]
       ]

-- Exercise 4:

exec :: [Instr] -> Stack -> Stack
exec [] stack = stack
exec (x : xs) stack = exec xs (exec1 x stack)

exercise4 :: Test
exercise4 =
  "exec"
    ~: [ exec [] [one] ~?= [one],
         exec [IPush one, IPush two, IAdd, IPush three, IAdd] []
           ~?= [six]
       ]

-- Exercise 5:

compile :: Exp -> [Instr]
compile (Num n) = [IPush n]
compile (Add e1 e2) = compile e1 ++ compile e2 ++ [IAdd]
compile (Branch e1 e2 e3) = compile e1 ++ [IBranch] ++ compile e2 ++ compile e3

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = 1

question :: String
question = "I noticed that it didn't work when I tried to use a guard within my case experession. Is there any way to do that or should I just use if then else?"

check :: Test
check =
  TestCase
    ( assertBool
        "fill in a time and question"
        ( time >= 0
            && question /= ""
        )
    )

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          exercise3,
          exercise4,
          check
        ]
  return ()

{-
These are just to make the tests easier to read; don't use them in your
solutions. Incidentally, this is an example of a very nice feature of Haskell:
definitions can appear after they are used!
-}

zero :: Nat
zero = Z

one :: Nat
one = S Z

two :: Nat
two = S (S Z)

three :: Nat
three = S (S (S Z))

six :: Nat
six = S (S (S (S (S (S Z)))))