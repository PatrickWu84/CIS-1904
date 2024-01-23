module Exercises where

import Test.HUnit
  ( Test (..),
    Testable (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )
import Prelude hiding (sum)

{- Read [instructions.md] first. -}

{-
Exercise 0: Uncomment each definition and fix the error so that it compiles.
Do not make any changes other than adding and removing parentheses.
-}

{-
In some languages, the syntax for defining a function and its arguments looks
something like f(x, y). Not so in Haskell! Do not wrap the arguments as a whole
in parentheses, and separate them with spaces instead of commas.
-}
-- f :: Int -> Int -> Int
-- f (x y) = x + y

{-
However, if a particular argument involves a pattern that itself has multiple
components, such as x : xs, that needs to wrapped in parentheses.
-}
-- g :: Int -> [Int] -> Int
-- g n [] = n
-- g n x : xs = n * x

{-
Analogous principles apply when using a function. Don't wrap the arguments as
a whole in parentheses, but do wrap individual arguments in parentheses as
needed. Add and remove parentheses below to make the result 12.
-}
-- result :: Int
-- result = g f (1 2) [4, 5] ++ [6, 7]

{-
Exercise 1: We first need to be able to break up a number into its last digit
and the rest of the number. Fill in the functions below:

Hint: Use `mod` for the first function and `div` for the second.
-}

lastDigit :: Int -> Int
lastDigit = error "unimplemented"

dropLastDigit :: Int -> Int
dropLastDigit = error "unimplemented"

{-
Here, we have some tests written using Haskell's unit testing library. For
example, the first one says the result of (lastDigit 1234) should be 4. You
are encouraged (but not required) to add your own!
-}
exercise1 :: Test
exercise1 =
  test
    [ "lastDigit"
        ~: [lastDigit 1234 ~?= 4],
      "dropLastDigit"
        ~: [dropLastDigit 1234 ~?= 123]
    ]

{-
Exercise 2: Now, we can break apart a number into its digits. It is actually
easier to break a number into a list of its digits in reverse order (can you
figure out why?). Fill in the function below:

For zero or negative inputs, toRevDigits should return the empty list.
-}

toRevDigits :: Int -> [Int]
toRevDigits = error "unimplemented"

exercise2 :: Test
exercise2 =
  "toRevDigits"
    ~: [ toRevDigits 1234 ~?= [4, 3, 2, 1],
         toRevDigits 0 ~?= []
       ]

{-
Exercise 3: Once we have the digits in the proper order, we need to double every
other one from left to right. Fill in the function below:
-}

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = error "unimplemented"

exercise3 :: Test
exercise3 =
  "doubleEveryOther"
    ~: [doubleEveryOther [1, 2, 3, 4] ~?= [1, 4, 3, 8]]

{-
Exercise 4: Next, we will want to sum together the *digits* in a list of
integers. Fill in the function below:
-}

sumDigits :: [Int] -> Int
sumDigits = error "unimplemented"

exercise4 :: Test
exercise4 =
  "sumDigits"
    ~: [ sumDigits [5, 10, 4, 18] ~?= 5 + 1 + 0 + 4 + 1 + 8,
         sumDigits [] ~?= 0
       ]

{-
Exercise 5: We are now ready to determine whether a credit card number is valid!
Fill in the function below:

Remember, don't repeat yourself: you should use the functions you defined in
previous exercises.
-}

validate :: Int -> Bool
validate = error "unimplemented"

exercise5 :: Test
exercise5 =
  "validate"
    ~: [ validate 5594589764218858 ~?= True,
         validate 1234567898765432 ~?= False
       ]

{-
Exercise 6: Write down the number of hours it took you to complete this
homework. Please also write one question you have about any of the material
we have covered so far.
-}

time :: Double
time = error "unimplemented"

question :: String
question = error "unimplemented"

exercise6 :: Test
exercise6 =
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
          exercise5,
          exercise6
        ]
  return ()
