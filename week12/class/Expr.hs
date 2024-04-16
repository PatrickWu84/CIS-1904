{-# LANGUAGE GADTs #-}

module Expr where

import Data.Kind (Type)

data Expr
  = I Int
  | B Bool
  | Add Expr Expr
  | And Expr Expr

-- data Expr where
--   I :: Int -> Expr
--   B :: Bool -> Expr
--   Add :: Expr -> Expr -> Expr
--   And :: Expr -> Expr -> Expr

-- eval :: Expr -> Maybe (Either Int Bool)
-- eval (I i) = Just (Left i)
-- eval (B b) = Just (Right b)
-- eval (Add e1 e2) = case (eval e1, eval e2) of
--   (Just (Left i1), Just (Left i2)) -> Just (Left (i1 + i2))
--   _ -> Nothing
-- eval _ = undefined

-- data Expr a where
--   I :: Int -> Expr Int
--   B :: Bool -> Expr Bool
--   Add :: Expr Int -> Expr Int -> Expr Int
--   And :: Expr Bool -> Expr Bool -> Expr Bool

-- Exercise: fill in this definition
-- eval :: Expr t -> t