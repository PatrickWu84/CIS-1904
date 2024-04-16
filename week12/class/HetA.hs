{-# LANGUAGE ExistentialQuantification #-}

module HetA where

data Itemizable = forall a. T a

items :: [Itemizable]
items =
  [ T 3,
    T "hello",
    T True
  ]
