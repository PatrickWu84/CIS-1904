{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module HetB where

data Itemizable = forall a. (Show a) => T a

items :: [Itemizable]
items =
  [ T 3,
    T "hello",
    T True
  ]

instance Show Itemizable where
  show :: Itemizable -> String
  show (T a) = show a