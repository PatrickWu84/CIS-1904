module Exercises where

{- Don't change the imports. -}
import Test.HUnit
import Prelude hiding (all, and)

{- Read [instructions.md] first.

   Please double check that you have precisely the `homework` folder open!
   You should see a type error in the definition of `foo` and *no*
   warnings or errors in the definition of `bar`. Make sure both of these things
   are true before continuing. Comment out `foo` and `bar` when you've confirmed.
-}

-- foo :: Int -> String
-- foo x = x + "hello"

-- bar :: Int -> String
-- bar x = foo x

-- Exercise -7:

remove7 :: [[Int]] -> [[Int]]
remove7 xss = map (filter (/= 7)) xss

exercise7 :: Test
exercise7 =
  "remove7"
    ~: [ remove7
           [ [7, 1, 2, 7],
             [3, 7],
             [4, 5, 6],
             [7, 7, 7]
           ]
           ~?= [ [1, 2],
                 [3],
                 [4, 5, 6],
                 []
               ]
       ]

-- Exercise 1:

all :: (a -> Bool) -> [a] -> Bool
all f xs = foldr (\x acc -> f x && acc) True xs

square :: [[a]] -> Bool
square xss = all (\xs -> length xs == length xss) xss

exercise1a :: Test
exercise1a =
  "all"
    ~: [ all id [] ~?= True,
         all even [2, 4] ~?= True,
         all even [1, 2, 3] ~?= False,
         all not [False, False] ~?= True
       ]

exercise1b :: Test
exercise1b =
  "square"
    ~: [ square jagged ~?= False,
         square perfect ~?= True,
         square [] ~?= True,
         square [[]] ~?= False
       ]

-- Exercise 2:

squarify :: [[a]] -> [[a]]
squarify [] = []
squarify xss = map (\xs -> take dim xs) (take dim xss)
  where
    dim :: Int
    dim = min (length xss) (minimum (map length xss))

{-
  type you searched for:
  FILL IN HERE
  Int -> [a] -> [a]

  name two functions with this type returned on Hoogle:
  FILL IN HERE
  take, drop
-}

exercise2 :: Test
exercise2 =
  "squarify"
    ~: [ squarify jagged ~?= [[1, 2], [5, 6]],
         squarify perfect ~?= perfect,
         squarify [[], [1, 2], [3]] ~?= [],
         squarify [[1, 2, 3], [4, 5, 6]] ~?= [[1, 2], [4, 5]]
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
question = "I tripped up a bit when using anonymous functions. Could you explain more about how to use them and when they are necessary?"

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
        [ exercise7,
          exercise1a,
          exercise1b,
          exercise2,
          check
        ]
  return ()

-- Examples from the instructions, for use in the tests: --

jagged :: [[Int]]
jagged =
  [ [1, 2, 3, 4],
    [5, 6],
    [7, 8, 9]
  ]

perfect :: [[Int]]
perfect =
  [ [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]