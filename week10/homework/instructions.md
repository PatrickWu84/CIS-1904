# Homework 8: Monads

**Due**: Monday, April 8 at 10 p.m.

## Exercise 1

Implement the functions with the provided type signatures. Write each function
twice, once with `do` notation and once using `>>=` directly. I am deliberately
not explaining in words what the functions do — let the types guide you!

## Exercise 2

In this exercise, we will examine a new context where monads are useful. Suppose
we want to maintain a log, perhaps for debugging purposes, as we compute some
result. We record both the log (of type `String`) and the final result (of type
`a`, for any `a`) in the custom data type

```
data Logger a = Logger String a
```

This is nice enough when we have just one computation, for example in the
definition of `square`. But if we want to _combine_ multiple computations that
might have logging statements, the code becomes clunkier than we would like.

For example, without the `Logger`, if we want to square a number twice, we would
be able to simply do `square . square`, but with the `Logger`, we instead need
something along these lines:

```
squareTwice :: Double -> Logger Double
squareTwice n =
  let Logger log1 n2 = square n
   in let Logger log2 n4 = square n2
    in Logger (log1 ++ "\n" ++ log2) n4
```

That is, we need to manually give a name to and combine the logs from each
computation. (Note that we can make this syntax _slightly_ nicer by combining
the `let`s, as shown in `Exercises.hs`, though the overall clunkiness persists.)

### Part (a)

Instead of manually combining these computations each time, we can abstract away
these details via a `Monad` instance for `Logger`.

In particular, `return a` should return a `Logger` with result `a` and an empty
log. And `(Logger log a) >>= k` should return a `Logger` whose result is the
result obtained from applying `k` on `a`, and whose log is the concatenation of
the original log and the log generated by `k`, separated by a newline.

Implement the `Monad` instance according to this specification. Then, take a
look at our new version of `squareTwice`, named `squareTwiceM`.

### Part (b)

Suppose we have a lengthier computation, where we use the Pythagorean theorem to
compute the hypotenuse. Implement `pythagoreanM` to do the same thing as
`pythagorean`, but take advantage of the `Monad` instance (and `do` notation) to
abstract away the details of accumulating the log.

### Part (c)

We might wonder why we don't just use `IO` to print debugging statements. One
reason is that the `Logger` monad, and its generalization, the `Writer` monad
(which works on accumulated values other than `String`s), allow more
fine-grained control over what to do with the log.

For example, we might write a function that takes in a boolean flag representing
verbosity and only prints the log when the flag is set to true. Let's try this —

First, re-implement the `Monad` library function `when`, which supports
conditional execution of a monadic computation — when the boolean flag is
`True`, then we return that computation; otherwise, we return the unit value
(i.e., a computation that does nothing).

Next, using `when`, fill in the unimplemented line of `printLogger` to satisfy
the behavior described above. That is, we should have

```
> printLogger True (pythagoreanM 3 4)
squared 3.0
squared 4.0
added 9.0 and 16.0
unsquared 25.0
5.0
> printLogger False (pythagoreanM 3 4)
5.0
```

## Grading

Please see Gradescope for the points breakdown. Exercises 1 and 2b will be both
auto- and manually graded, and Exercises 2a and 2c will be autograded. There
will also be two points for general style.