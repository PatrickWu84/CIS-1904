# Homework 3: Recursion Patterns

**Due**: Monday, Feb. 6 at 10 p.m.

In this homework, we will use `map`, `filter`, and `fold` to define functions
that operate on lists of lists. While they share this common theme, you should
not need to use any code from one exercise in another.

Also, except when prompted to in Exercise 2, you should not need to use any
library functions that operate on lists other than `map`, `filter`, `fold`, and
`length`. Please do not make any modifications to the imports.

Recall the convention of naming a term of type `[a]` as `xs`, and the elements
inside (of type `a`) as `x`. When we instead have type `[[a]]`, the convention
is to name the terms as `xss` and the elements inside (of type `[a]`) as `xs`.

**Constraint**: You may _not_ write any recursive functions in this homework.

## Exercise -7

We start with a very silly function on lists of lists of integers:

Suppose we hate the number seven. Implement `remove7` to rid the input of 7s.

## Exercise 1

Let's introduce some terminology.

Given a list of lists `xss`, we say its _height_ is the number of elements (e.g.
rows) in `xss`. For example, the height of
```
[[1, 2, 3, 4],
 [5, 6],
 [7, 8, 9]]
```
is three, since it contains three lists.

Each row `xs` in `xss` has some _width_, which is the number of elements in
`xs`. In the above example, the widths are four, two, and three.

We say `xss` is _square_ if all of the widths are equal to the height. The above
example is not square, but the below is.
```
[[1, 2, 3],
 [4, 5, 6],
 [7, 8, 9]]
```

### Part (a)

First, implement `all`, where `all f xs` determines whether all elements in `xs`
satisfy the boolean predicate `f`. **Constraint**: Use `foldr`.

### Part (b)

Then, implement `square`, which determines whether a list of lists is square.
**Constraint**: Use `all`.

## Exercise 2

In this exercise, we will force a list of lists to be a square. For example,

```
[[1, 2, 3, 4],
 [5, 6],
 [7, 8, 9]]
```

should be `squarify`ed into

```
[[1, 2],
 [5, 6]]
```

### Part (a)

To determine the dimension `dim` of the resulting square, we take the minimum of
the original's widths and height. Implement `dim`, with the help of the `min`
function, where `min m n` returns the lower of numbers `m` and `n`.

Notice the use of `where`, which allows us to define things locally to be used
in the body of the definition above.

### Part (b)

We'll want to use a library function that returns the first `n` elements of a
list, for some number `n`. What would be the _type_ of such a function?

Search the type on [Hoogle](https://hoogle.haskell.org/) — make sure you select
`category:Prelude` from the dropdown. If you searched for the right type, you
should see two distinct functions with this same type, one of which will be
useful to us. Fill in the parts that say "FILL IN HERE" in `Exercises.hs`.

### Part (c)

Implement `squarify`, using parts (a) and (b). Squarification should happen by
only keeping the first `dim` elements in each row, and the first `dim` rows.

You should not need to check if a list of lists is square before squarifying it
— your implementation should work regardless.

## Grading

Please see Gradescope for the points breakdown. I will pay particular attention
to whether you followed the constraints in the instructions and whether you made
effective use of functions such as `map`, `filter`, and `fold`.