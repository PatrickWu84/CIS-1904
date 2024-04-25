module Lib (someFunc, otherFunc, Color (..)) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

otherFunc :: IO ()
otherFunc = putStrLn "otherFunc"

data Color = Red | Blue | Green