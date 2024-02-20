module Exercises where

import Prelude hiding (const, flip)

-- example: refactoring the code to use Maybe

predict :: WeatherRequest -> WeatherForecast
predict (ByLocation "Philly") = Forecast Sunny
predict (ByCoordinate 90 _) = Forecast Snowy
predict _ = Unknown

data Weather
  = Sunny
  | Cloudy
  | Windy
  | Rainy
  | Snowy
  deriving (Show)

data WeatherRequest
  = ByLocation String
  | ByCoordinate Int Int
  deriving (Show)

data WeatherForecast
  = Forecast Weather
  | Unknown
  deriving (Show)

-- exercise: implement these functions

const :: a -> b -> a
const = error "unimplemented"

flip :: (a -> b -> c) -> b -> a -> c
flip = error "unimplemented"