# Homework 9: Property-Based Testing

**Due**: Monday, April 15 at 10 p.m.

## Background

This homework is on binary-search trees (BSTs). Recall that in a BST, at any
node with some value `x`, the left subtree has values less than `x`, and the
right subtree has values greater than `x`. For example,

```Haskell
Branch (Branch Leaf 3 Leaf) 4 (Branch Leaf 5 Leaf)
```

is a valid BST. On the other hand,

```Haskell
Branch (Branch Leaf 4 Leaf) 3 (Branch Leaf 5 Leaf)
```

is invalid. If you have seen similar data structures before, please note that
BSTs are _not_ required to be balanced.

The implementations of several useful functions can be found in `BST.hs`.

## Exercise 1

In this exercise, we will implement a generator for BSTs. We expect that since
these trees are a recursive data type, the generator will also need to
recursively generate the subtrees. But how do we enforce that the left subtree
has smaller values than the current node and the right subtree greater values?

One approach is to maintain a lower and upper bound, where `genTree lower upper`
generates BSTs with values strictly between `lower` and `upper`. Fill in the
unimplemented parts by considering the following:

1. When `lower > upper`, we must generate a `Leaf`.
2. Otherwise, we can either make a `Leaf` or a `Branch`. Use the `frequency`
   combinator to assign weights to each case.
3. The `Leaf` case is straightforward.
4. In the `Branch` case, first generate a value `x` between `lower` and `upper`.
   Find a combinator from `QuickCheck` to help with this!
5. Then, make recursive calls to `genTree` to generate the left and right
   subtrees, taking care to update the bounds appropriately.

If your generator is correct, then `quickCheck prop_ArbitraryValid` should pass.

## Exercise 2

The function `find :: Int -> Tree -> Bool` determines whether or not a value is
in a tree. How can we write a property specifying `find`?

In words, we want `find x t` to return `True` if `x` is in `t` and `False` if
not. But how do we know whether `x` is in `t`? To determine that for an arbitrary
tree, we would need a function like `find`!

This problem demonstrates that we often do not want to test functions in
isolation. Instead, it can be useful to think about the relationships between
multiple functions — in this case, `BST.hs` also contains `insert`, which
inserts a value into a tree, and `delete`, which deletes a value from a tree.

Fill in these two properties:

1. `prop_FindPostPresent` should check that we can find a value we inserted.
2. `prop_FindPostAbsent` should check that we cannot find a value we deleted.

Running `quickCheck` on both properties should pass. Of course, trivial
properties will also pass, so make sure that your properties actually match the
above specification. You may want to deliberately break the `find`
implementation in `BST.hs` and check that your properties fail.

## Grading

Submit only `Exercises.hs` to Gradescope.

This homework will be graded out of 20 points. Exercise 1 is mostly autograded.
Exercises 2 is mostly manually graded, to check whether you implemented the
desired properties. See Gradescope for the full breakdown.
