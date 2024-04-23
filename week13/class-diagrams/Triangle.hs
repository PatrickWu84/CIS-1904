{-# LANGUAGE NoMonomorphismRestriction #-}

module Triangle where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

-- https://diagrams.github.io/gallery/Sierpinski.html

{- ORMOLU_DISABLE -}
sierpinski :: Int -> Diagram B
sierpinski 1 = triangle 1
sierpinski n =    s
                 ===
              (s ||| s) # centerX
  where
    s = sierpinski (n - 1)
{- ORMOLU_ENABLE -}

main :: IO ()
main = mainWith (sierpinski 7 # pad 1.1)

-- :main -w 200 -h 200 -o triangle.svg