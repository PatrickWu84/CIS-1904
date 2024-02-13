# Homework 4: Higher-Order Patterns

**Due**: Monday, Feb. 19 at 10 p.m.

## Exercise 0

In this exercise and the next, we will practice refactoring code to more closely
obey the idiom that functions should not unnecessarily refer to their arguments
— instead, we can use and manipulate them directly.

For each function, you should take care that you are not changing its
functionality. (This likely means you should first carefully read the definition
and make sure you understand it!)

### or

We'll make two changes to the definition of `or`.

First, notice that we are giving a name to the argument `xs` only to pass it in
directly to the `foldr`. We can "eta reduce" by removing `xs` from both sides.

Second, notice that constructing an anonymous function to pass into `foldr` is
actually unnecessary. What can we directly use instead?

### or2

After making these changes to `or` manually, remove the two "HLINT ignore"
lines. You should not see any warnings under your modified `or`, but you
should see warnings appear under the yet unmodified `or2`.

Hover over the warnings, scroll until you see "Quick fix" and then "Apply hint."
You should end up with the same code as `or`.

For the rest of the assignment, you can use `hlint` to help you make these two
kinds of changes — but you should make sure you understand why the changes work.

## Exercise 1

Next, we will practice using the composition operator. Note that in general,
`hlint` is unable to automatically propose this type of refactoring.

Modify the definition of `any` so that it uses `.` and no longer needs to refer
to the argument `xs`. (But it can still refer to the argument `f`).

Modify the definition of `bigEnough` by rewriting `(\x -> abs x >= n)` as the
composition of two functions, so that it no longer needs to refer to an argument
`x`. Recall the _operator sections_ we discussed in class.

## Exercise 2

Reimplement `concat` and `concatMap` from the list library. `concat` should
concatenate the elements in a list of lists. `concatMap` should map a function
over the elements of a list and concatenate the results.

What's the shortest solution you can come up with using the techniques we've
learned? (Whitespace doesn't count.)

## Exercise 3

Take a moment to read `func`. It probably hurts to look at now! Reimplement
`func` as `func'` in more idiomatic Haskell style. Use wholemeal programming
practices, breaking the function into a pipeline of incremental transformations.

You may want to use the library function `sum`, which sums together a list of
numbers. You may also want to use some subset of `map`, `filter`, and `fold`.

Make sure that your new function still does the same thing! Feel free to write
your own tests if you like.

## Grading

Please see Gradescope for the points breakdown.

As you may notice, this homework is largely manually graded due to the nature of
the exercises. If you would like to check your answers, I am happy to provide
feedback within 24 hours of your initial submission, and you can optionally
choose to revise and resubmit. Just make a private post on Ed! Please note that
the usual deadline still applies to your resubmission.