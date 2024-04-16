{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module TempB where

{- phantom types with data kinds: -}

-- example from Haskell in Depth
newtype Temp (a :: TempUnit) = Temp Double
  deriving (Num)

data TempUnit = F | C

tempUSA :: Temp F
tempUSA = Temp 70

tempWorld :: Temp C
tempWorld = Temp 20

example :: Temp F
example = tempUSA + tempUSA

-- silly :: Temp Bool
-- silly = Temp 42