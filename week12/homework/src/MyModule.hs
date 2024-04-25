module MyModule where

import Data.Maybe (isJust)
import Lib (Color (..), otherFunc, someFunc)

foo :: Bool
foo = isJust (Just "hello")

s :: IO ()
s = someFunc

o :: IO ()
o = otherFunc

func :: Color -> Color
func c = c

func2 :: Color -> Color
func2 Red = Green
func2 Blue = Red
func2 Green = Blue