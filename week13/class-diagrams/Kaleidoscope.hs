{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad (replicateM)
import Control.Monad.Random
import Data.Colour.Palette.ColorSet
import Data.List (zipWith, zipWith3)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import System.Random

-- https://diagrams.github.io/gallery/Kaleidoscope.html

kaleidoscope :: Diagram B -> Diagram B
kaleidoscope d = appends hex hexs
  where
    hexs = zip dirs (replicate 6 hex)
    dirs = iterate (rotateBy (1 / 6)) (rotateBy (1 / 12) unitX)
    hex = (mconcat . take 6) (iterateIdx next tri)
    tri = alignBR (cutTriangle d)
    next i = reflectAbout origin (rotateBy (fromIntegral i / 6) xDir)

cutTriangle :: Diagram B -> Diagram B
cutTriangle = clipped (triangle 1)

confettiScope :: Int -> Int -> Diagram B
confettiScope n r =
  kaleidoscope (mkConfetti n (mkStdGen r))
    # pad 1.1
    # lw none

mkConfetti :: Int -> (StdGen -> Diagram B)
mkConfetti n = evalRand (confetti n)

confetti :: Int -> Rand StdGen (Diagram B)
confetti n = do
  rs <- replicateM n sizeValue -- radius
  cs <- replicateM n getRandom -- color index
  os <- replicateM n getRandom -- opacity
  xs <- replicateM n coordValue -- x coordinate
  ys <- replicateM n coordValue -- y coordinate
  let mkCircle :: Double -> Int -> Double -> Diagram B
      mkCircle r c o = circle r # fc (webColors c) # opacity o
      circles = zipWith3 mkCircle rs cs os
      positions = zipWith mkP2 xs ys
  return (position (zip positions circles))

iterateIdx :: (Int -> a -> a) -> a -> [a]
iterateIdx f t = go f t 0
  where
    go f t i =
      let t' = f i t
       in t' : go f t' (i + 1)

sizeValue :: (RandomGen g) => Rand g Double
sizeValue = getRandomR (0.05, 0.25)

coordValue :: (RandomGen g) => Rand g Double
coordValue = getRandomR (-0.5, 0.5)

main :: IO ()
main = mainWith (confettiScope 40 1940)

-- :main -w 200 -h 200 -o kale.svg