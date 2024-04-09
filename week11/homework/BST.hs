module BST where

data Tree
  = Leaf
  | Branch Tree Int Tree
  deriving (Eq, Show)

-- Look at isBST before Exercise 1.

isBST :: Tree -> Bool
isBST Leaf = True
isBST (Branch l x r) =
  isBST l
    && isBST r
    && all (< x) (toList l)
    && all (> x) (toList r)

toList :: Tree -> [Int]
toList Leaf = []
toList (Branch l x r) = toList l ++ [x] ++ toList r

-- Look at find, insert, and delete before Exercise 2.

find :: Int -> Tree -> Bool
find _ Leaf = False
find x (Branch l x' r)
  | x < x' = find x l
  | x > x' = find x r
  | otherwise = True

insert :: Int -> Tree -> Tree
insert x Leaf = Branch Leaf x Leaf
insert x (Branch l x' r)
  | x < x' = Branch (insert x l) x' r
  | x > x' = Branch l x' (insert x r)
  | otherwise = Branch l x r

delete :: Int -> Tree -> Tree
delete _ Leaf = Leaf
delete x (Branch l x' r)
  | x < x' = Branch (delete x l) x' r
  | x > x' = Branch l x' (delete x r)
  | otherwise = join l r

join :: Tree -> Tree -> Tree
join Leaf r = r
join l Leaf = l
join (Branch l x r) (Branch l' x' r') =
  Branch l x (Branch (join r l') x' r')
