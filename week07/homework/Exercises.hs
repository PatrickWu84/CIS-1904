{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Data.List
import Test.HUnit

{- Read [instructions.md] first. -}

newtype Poly = P [Int]

-- Exercise 1

-- Add the instance of Eq here.

instance Eq Poly where
  (==) :: Poly -> Poly -> Bool
  (P c1) == (P c2) = removeZeroCoffs c1 == removeZeroCoffs c2
    where
      removeZeroCoffs :: [Int] -> [Int]
      removeZeroCoffs = reverse . dropWhile (== 0) . reverse

exercise1 :: Test
exercise1 =
  "eq"
    ~: [ P [1, 2, 3] == P [1, 2, 3] ~?= True,
         P [1, 2] == P [1, 2, 3] ~?= False,
         P [1, 2, 0] == P [1, 2] ~?= True,
         P [] == P [0, 0] ~?= True,
         P [1, 0, 2] == P [1, 2] ~?= False
       ]

-- Exercise 2

instance Show Poly where
  show :: Poly -> String
  show (P []) = "0"
  show (P cs) = intercalate " + " (zipWith showTerm cs [0 ..])
    where
      showTerm :: Int -> Int -> String
      showTerm c e
        | e == 0 = show c
        | e == 1 = show c ++ "x"
        | otherwise = show c ++ "x^" ++ show e

exercise2 :: Test
exercise2 =
  "show"
    ~: [ show (P [1, 2, 3]) ~?= "1 + 2x + 3x^2",
         show (P [-1, 0]) ~?= "-1 + 0x",
         show (P []) ~?= "0"
       ]

-- Exercise 3

instance Num Poly where
  -- These will be implemented later.

  (+) :: Poly -> Poly -> Poly
  (+) = plus

  (*) :: Poly -> Poly -> Poly
  (*) = times

  -- Implement these now.
  negate :: Poly -> Poly
  negate (P cs) = P (map negate cs)

  fromInteger :: Integer -> Poly
  fromInteger x = P [fromInteger x]

  -- Leave these unimplemented;
  -- no meaningful definition exists.

  abs :: Poly -> Poly
  abs = undefined

  signum :: Poly -> Poly
  signum = undefined

exercise3a :: Test
exercise3a =
  "negate"
    ~: [ negate (P [1, 2, 3]) ~?= P [-1, -2, -3],
         negate (P [-1, 0]) ~?= P [1, 0],
         negate (P []) ~?= P []
       ]

exercise3b :: Test
exercise3b =
  "fromInteger"
    ~: [ 3 ~?= P [3],
         0 ~?= P [0]
       ]

-- Exercise 4

plus :: Poly -> Poly -> Poly
plus (P cs1) (P cs2) = P (zipWith (+) (pad cs1) (pad cs2))
  where
    pad :: [Int] -> [Int]
    pad cs = cs ++ replicate (length cs2 - length cs) 0

exercise4 :: Test
exercise4 =
  "sum"
    ~: [ P [0, 1, 2] + P [1, 0, 2] ~?= P [1, 1, 4],
         P [5, 1] + P [1, 1, 3] ~?= P [6, 2, 3],
         P [1, 1, 3] + P [5, 1] ~?= P [6, 2, 3],
         P [] + P [] ~?= P []
       ]

-- Exercise 5

times :: Poly -> Poly -> Poly
times (P c1) (P c2) = sum (go c1 c2)
  where
    go :: [Int] -> [Int] -> [Poly]
    go [] _ = []
    go (c1 : c1s) c2s = P (map (c1 *) c2s) : go c1s (0 : c2s)

exercise5 :: Test
exercise5 =
  "times"
    ~: [ P [1, 1, 1] * P [2, 2] ~?= P [2, 4, 4, 2],
         P [2, 2] * P [1, 1, 1] ~?= P [2, 4, 4, 2],
         P [1, 2, 3] * P [4, 5, 6] ~?= P [4, 13, 28, 27, 18],
         P [] * P [1, 2, 3] ~?= P []
       ]

-- Tying it all together:

x :: Poly
x = P [0, 1]

final :: Test
final = (2 * x - 1) * (x + 2) == (2 * x ^ 2 + 3 * x - 2) ~?= True

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = 2

question :: String
question = "I got confused for exercise 3 because of how negate/fromInteger is defined to be of a certain type, but then you can still call it on a different type (defined poly, gets called on int). Can you explain more how that works?"

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
          exercise3a,
          exercise3b,
          -- these are optional:
          -- exercise4,
          -- exercise5,
          -- final,
          check
        ]
  return ()