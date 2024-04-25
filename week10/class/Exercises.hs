module Exercises where

import Data.List (elemIndex, lookup)
import Prelude hiding (mapM)

directory :: [(String, String)]
directory =
  [ ("Jill", "jill3@upenn.edu"),
    ("Jack", "jack4@other.edu"),
    ("Bob", "malformed-email")
  ]

-- Follow along when we convert to better notation!
getUsername :: String -> Maybe String
getUsername name =
  case lookup name directory of
    Nothing -> Nothing
    Just email ->
      case elemIndex '@' email of
        Nothing -> Nothing
        Just i -> Just (take i email)

{-
Exercise:
Get the third element of a list. Use safeHead and safeTail.
e.g. [1, 2, 3, 4] -> Just 3 and [1, 2] -> Nothing.
-}

safeThird :: [a] -> Maybe a
safeThird as =
  case safeTail as of
    Nothing -> Nothing
    Just as' -> case safeTail as' of
      Nothing -> Nothing
      Just as'' -> safeHead as''

safeThird' :: [a] -> Maybe a
safeThird' as = do
  as' <- safeTail as
  as'' <- safeTail as'
  safeHead as''

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : as) = Just as

-------

-- Follow along when we convert to better notation!
cross :: [a] -> [b] -> [(a, b)]
cross xs ys =
  concatMap
    ( \x ->
        concatMap
          (\y -> [(x, y)])
          ys
    )
    xs

{-
Exercise:
Compute the factors of a number. Use list comprehension.
e.g. 15 -> [1, 3, 5, 15]
-}

factors :: Int -> [Int]
factors x = [n | n <- [1 .. x], x `mod` n == 0]

-------

{-
Exercise:
Implement mapM using sequence (and another function).
-}

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f
