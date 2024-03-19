{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Test.HUnit
import Prelude hiding (unzip, (<$))

{- Read [instructions.md] first. -}

-- Exercise 1:

fconst :: (Functor f) => b -> f a -> f b
fconst = error "unimplemented"

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip = error "unimplemented"

-- Exercise 2:

data RoseTree a = Node a [RoseTree a]
  deriving (Eq, Show)

instance Foldable RoseTree where
  foldr = error "unimplemented"

exercise2 :: Test
exercise2 =
  "rosetree"
    ~: [ and (Node True []) ~?= True,
         and (Node False []) ~?= False,
         and (Node True [Node True [Node False [Node True []]]]) ~?= False,
         foldr (:) [] (Node 1 [Node 2 [Node 3 []], Node 4 []]) ~?= [1, 2, 3, 4]
       ]

-- Exercise 3:

-- Modify this definition.
data RoseTreeG a = NodeG a [RoseTreeG a]

-- Copy and paste your Foldable instance, and modify it for RoseTreeG.

-- Uncomment this test. It should type check and pass.
-- exercise3 :: Test
-- exercise3 =
--   "general-rosetree"
--     ~: [ -- a tree of Maybes
--          sum (NodeG 1 Nothing) ~?= 1,
--          sum (NodeG 1 (Just (NodeG 2 (Just (NodeG 3 Nothing))))) ~?= 6,
--          -- a tree of lists
--          foldr (:) [] (NodeG 1 [NodeG 2 [NodeG 3 []], NodeG 4 []]) ~?= [1, 2, 3, 4]
--        ]