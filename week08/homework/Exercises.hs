{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Test.HUnit
import Prelude hiding (unzip, (<$))

{- Read [instructions.md] first. -}

-- Exercise 1:

fconst :: (Functor f) => b -> f a -> f b
fconst x = fmap (const x)

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip f = (fmap fst f, fmap snd f)

-- Exercise 2:

data RoseTree a = Node a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap f (Node a ts) = Node (f a) (fmap (fmap f) ts)

exercise2 :: Test
exercise2 =
  "fmap-rosetree"
    ~: [ fmap (+ 3) (Node 2 []) ~?= Node 5 [],
         fmap (+ 3) (Node 1 [Node 2 [Node 3 []], Node 4 []]) ~?= Node 4 [Node 5 [Node 6 []], Node 7 []]
       ]

-- Exercise 3:

instance Foldable RoseTree where
  foldr :: (a -> b -> b) -> b -> RoseTree a -> b
  foldr f b (Node a ts) = f a (foldr (flip (foldr f)) b ts)

exercise3 :: Test
exercise3 =
  "fold-rosetree"
    ~: [ and (Node True []) ~?= True,
         and (Node False []) ~?= False,
         and (Node True [Node True [Node False [Node True []]]]) ~?= False,
         foldr (:) [] (Node 1 [Node 2 [Node 3 []], Node 4 []]) ~?= [1, 2, 3, 4]
       ]

-- Exercise 4:

-- Modify this definition.
data RoseTreeG a = NodeG a [RoseTreeG a]

-- Copy and paste your Foldable instance, and modify it for RoseTreeG.

-- Uncomment this test. It should type check and pass.
-- exercise4 :: Test
-- exercise4 =
--   "general-rosetree"
--     ~: [ -- a tree of Maybes
--          sum (NodeG 1 Nothing) ~?= 1,
--          sum (NodeG 1 (Just (NodeG 2 (Just (NodeG 3 Nothing))))) ~?= 6,
--          -- a tree of lists
--          foldr (:) [] (NodeG 1 [NodeG 2 [NodeG 3 []], NodeG 4 []]) ~?= [1, 2, 3, 4]
--        ]

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = 1.5

question :: String
question = "what are the use cases for a rose tree?"

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
        [ exercise2,
          -- these are optional:
          -- exercise3,
          -- exercise4,
          -- final,
          check
        ]
  return ()