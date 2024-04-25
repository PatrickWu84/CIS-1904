module Exercises where

{- Don't change the imports. -}
import Test.HUnit
import Prelude hiding (any, concat, concatMap, or)

{- Read [instructions.md] first. -}

-- Exercise 0:

or :: [Bool] -> Bool
or = foldr (||) False

or2 :: [Bool] -> Bool
or2 = foldr (||) False

-- Exercise 1

any :: (a -> Bool) -> [a] -> Bool
any f = or . map f

bigEnough :: Int -> [Int] -> [Int]
bigEnough n = filter ((>= n) . abs)

-- Exercise 2:

concat :: [[a]] -> [a]
concat = foldr (++) []

exercise2a :: Test
exercise2a =
  "concat"
    ~: [ concat [] ~?= ([] :: [()]),
         concat [[1]] ~?= [1],
         concat [[1, 2], [], [3], [4, 5], []] ~?= [1, 2, 3, 4, 5]
       ]

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

exercise2b :: Test
exercise2b =
  "concatMap"
    ~: [ concatMap f [] ~?= [],
         concatMap f [1] ~?= [1, 1],
         concatMap f [1, 2, 3] ~?= [1, 1, 2, 2, 3, 3]
       ]
  where
    f :: Int -> [Int]
    f = replicate 2

-- Exercise 3:

func :: [Int] -> Int
func [] = 0
func (x : xs)
  | even x = (x * 3) + func xs
  | otherwise = func xs

func' :: [Int] -> Int
func' = sum . map (* 3) . filter even

exercise3b :: Test
exercise3b =
  "func'"
    ~: [ func' [] ~?= 0,
         func' [1, 2, 3, 4, 5] ~?= 18
       ]

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = 1

question :: String
question = "What are the basics of the test framework in haskell?"

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
        [ exercise2a,
          exercise2b,
          check
        ]
  return ()
