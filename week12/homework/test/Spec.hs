{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck



sort :: [Int] -> [Int]
prop_SortOrdered :: [Int] -> Bool
prop_SortOrdered xs = ordered (sort xs)
  where ordered [] = True
        ordered [_] = True
        ordered (x:y:zs) = x <= y && ordered (y:zs)
        
prop_SortLength :: [Int] -> Bool
prop_SortLength xs = length xs == length (sort xs)

return []
main :: IO Bool
main = $quickCheckAll
