{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Exercises where

import System.IO.Silently (capture_)
import Test.HUnit

-- Exercise 1:

fish1 :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
fish1 f g a = do
  b <- f a
  g b

fish2 :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
fish2 f g a = f a >>= g

join1 :: (Monad m) => m (m a) -> m a
join1 mma = do
  ma <- mma
  ma

join2 :: (Monad m) => m (m a) -> m a
join2 mma = mma >>= id

-- Exercise 2:

data Logger a = Logger String a
  deriving (Eq, Show)

square :: Double -> Logger Double
square n = Logger ("squared " ++ show n) (n * n)

squareTwice :: Double -> Logger Double
squareTwice n =
  let Logger log1 n2 = square n
      Logger log2 n4 = square n2
   in Logger (log1 ++ "\n" ++ log2) n4

-- Exercise 2a:

instance Monad Logger where
  return :: a -> Logger a
  return = Logger ""

  (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  (Logger log a) >>= k = let Logger log' b = k a in Logger (log ++ "\n" ++ log') b

squareTwiceM :: Double -> Logger Double
squareTwiceM n = square n >>= square

exercise2a :: Test
exercise2a =
  "2a"
    ~: [squareTwiceM 17 ~?= squareTwice 17]

-- Exercise 2b:

add :: Double -> Double -> Logger Double
add m n = Logger ("added " ++ show m ++ " and " ++ show n) (m + n)

root :: Double -> Logger Double
root n = Logger ("unsquared " ++ show n) (sqrt n)

pythagorean :: Double -> Double -> Logger Double
pythagorean a b =
  let Logger log1 a2 = square a
      Logger log2 b2 = square b
      Logger log3 c2 = add a2 b2
      Logger log4 c = root c2
   in Logger (log1 ++ "\n" ++ log2 ++ "\n" ++ log3 ++ "\n" ++ log4) c

pythagoreanM :: Double -> Double -> Logger Double
pythagoreanM a2 b2 = do
  a2 <- square a2
  b2 <- square b2
  c2 <- add a2 b2
  root c2

exercise2b :: Test
exercise2b =
  "2b"
    ~: [pythagoreanM 3 4 ~?= pythagorean 3 4]

-- Exercise 2c:

when :: (Monad m) => Bool -> m () -> m ()
when bool a = if bool then a else return ()

printLogger :: (Show a) => Bool -> Logger a -> IO ()
printLogger verbose (Logger log result) = do
  when verbose (putStrLn log)
  print result

exercise2c :: Test
exercise2c =
  "printLogger"
    ~: [ testPrint
           (when True (print 3))
           "3\n",
         testPrint
           (when False (print 3))
           "",
         testPrint
           (printLogger True (square 3.0))
           "squared 3.0\n9.0\n",
         testPrint
           (printLogger False (square 3.0))
           "9.0\n"
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
question = "Is it more common to use do notation or >>=?"

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
          exercise2c,
          check
        ]
  return ()

{-
Note that Haskell enforces that in order to be a Monad, one also has to be a
Functor and an Applicative. To allow us to focus on monads, we leave these
unimplemented in this homework. Please don't modify the two lines below.
-}

instance Functor Logger

instance Applicative Logger

-- helper code for testing

testPrint :: IO () -> String -> IO ()
testPrint io expected = do
  out <- capture_ io
  assertEqual "" out expected