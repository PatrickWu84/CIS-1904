module Exercises where

data Weather
  = Sunny
  | Cloudy
  | Windy
  | Rainy
  | Snowy
  deriving (Show)

data WeatherRequest
  = ByLocation String
  | ByCoordinate Coord
  deriving (Show)

data WeatherForecast
  = Forecast Weather
  | Unknown
  deriving (Show)

----- dividing line -----

data Coord = Coord Int Int
  deriving (Show)

c1 :: Coord
c1 = Coord 90 0

c2 :: Coord
c2 = Coord 0 90

{- Exercise:
   Write a function that (point-wise) adds two coordinates,
   e.g. addCoord c1 c2 should be Coord 90 90. -}

{- Exercise:
   Rewrite WeatherRequest and prediction to use Coord
   instead of Int Int.

   The logic should otherwise be the same, e.g.
   prediction (ByCoordinate c1) should be Forecast Snowy and
   prediction (ByCoordinate c2) should be Unknown. -}

----- dividing line -----

data Tree
  = Leaf
  | Node Tree Int Tree
  deriving (Show)

tree :: Tree
tree = Node Leaf 1 (Node Leaf 2 Leaf)

{- Exercise:
   size t should evaluate to the number of leaves in t.
   example: size tree should be 2. -}

{- Exercise:
   add3Tree t should add 3 to every node value in t.
   example: add3Tree tree should be Node Leaf 4 (Node Leaf 5 Leaf). -}