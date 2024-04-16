{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Vec where

import Data.Kind (Type)

data Nat = Z | S Nat

data List (n :: Nat) a where
  Nil :: List Z a
  Cons :: a -> List n a -> List (S n) a

example :: List (S (S Z)) Int
example = Cons 3 (Cons 4 Nil)

-- nonexample :: List Z Int
-- nonexample = Cons 3 (Cons 4 Nil)

head :: List (S n) a -> a
head (Cons a _) = a

tail :: List (S n) a -> List n a
tail (Cons _ as) = as

type (+) :: Nat -> Nat -> Nat
type family m + n where
  Z + n = n
  S m + n = S (m + n)

append :: List m a -> List n a -> List (m + n) a
append Nil bs = bs
append (Cons a as) bs = Cons a (append as bs)