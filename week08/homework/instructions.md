# Homework 6: Functor, Foldable

**Due**: Monday, March 24 at 10 p.m.

## Overview

The purpose of this homework is to practice reading and writing Haskell code at
a high level of abstraction, with heavy generalization over type variables.

Approach these problems by reading the type signature, thinking about the types
of the things (functions, arguments, etc.) that you have available to you, and
then figuring out how to combine them in a way that gets you the desired type.

## Exercise 1

To practice with the `Functor` type class, implement `fconst` and `unzip`. There
are deliberately no test cases, because any (safe) implementation that type
checks will be correct.

## Exercise 2

Up until now, the various trees we have worked with have been binary trees. A
alternative data structure is the rose tree, where each node instead has a list
of children. e.g.

```Haskell
data RoseTree a = Node a [RoseTree a]
```

Implement a `Functor` instance for `RoseTree`.

Constraint: You may **not** pattern-match over the list `ts`. Instead, you
should use the `fmap` (or equivalently, `map`) for lists.

## Exercise 3 (Optional)

Next, we practice with the `Foldable` type class.
Implement a `Foldable` instance for `RoseTree`.

I strongly recommend that you make use of typed holes (via underscores) to
*incrementally* write this function. For example, we should have

```Haskell
foldr f b (Node a ts) = f _ _
```

Figure out what the first argument to `f` should be: What is its type? What do
we have of that type? Then, figure out the second argument: What is its type?
How can we (again, incrementally!) build something of that type?

Constraint: You may **not** pattern-match over the list `ts`. Instead, you
should use the `foldr` for lists.

## Exercise 4 (Optional)

We can further generalize `RoseTree`. Modify `RoseTreeG` to take another type
parameter `t`, so that `NodeG`s contain not a list of children but a `t` of
children. (Note that while normal rose trees are a common and useful data
structure, this exercise is just for fun.)

Copy and paste your `Foldable` instance, and adapt it for `RoseTreeG`. You
should be able to do this in a way where you're only modifying the types and not
the implementation.

(You can also do this for `Functor`, if you like. This problem was just
originally written when the assignment focused on `Foldable`.)