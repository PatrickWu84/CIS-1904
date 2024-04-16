{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TempA where

import Data.Data (Proxy (..))

{- phantom types: -}

-- example from Haskell in Depth
newtype Temp a = Temp Double
  deriving (Num)

data F

data C

tempUSA :: Temp F
tempUSA = Temp 70

tempWorld :: Temp C
tempWorld = Temp 20

example :: Temp F
example = tempUSA + tempUSA

-- nonexample :: Temp F
-- nonexample = tmempUSA + tempWorld

class ShowUnit a where
  showUnit :: Proxy a -> String

instance ShowUnit F where
  showUnit :: Proxy F -> String
  showUnit _ = "F"

instance ShowUnit C where
  showUnit :: Proxy C -> String
  showUnit _ = "C"

instance (ShowUnit a) => Show (Temp a) where
  show :: Temp a -> String
  show (Temp d) = show d ++ " " ++ showUnit (Proxy :: Proxy a)

silly :: Temp Bool
silly = Temp 42
