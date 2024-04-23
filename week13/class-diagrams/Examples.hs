{-# LANGUAGE NoMonomorphismRestriction #-}

module Examples where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

example1 :: Diagram B
example1 = circle 2

example2 :: Diagram B
example2 = triangle 1

example3 :: Diagram B
example3 = circle 1 `atop` circle 2

example4 :: Diagram B
example4 =
  beside
    (r2 (2, 3))
    (circle 1)
    (circle 2)

example5 :: Diagram B
example5 =
  (circle 1 # fc orange)
    ||| (circle 1 # fc purple)

main :: IO ()
main = mainWith (example5 # pad 1.1)
