# Homework 5: Type Classes

**Due**: Monday, March 17 at 10 p.m.

## Overview

We will work with polynomials in this homework. A polynomial is simply a
sequence of terms, and each term has a _coefficient_ and a _degree_. For
example, the polynomial `3 + 5x + x^2` has three terms, one of degree 0 with
coefficient 3, one of degree 1 with coefficient 5, and one of degree 2 with
coefficient 1.

In our representation, we will avoid explicitly specifying the degrees of terms
by representing a polynomial as a list of coefficients, each of which has degree
equal to its position in the list.

```Haskell
newtype Poly = P [Int]
```

In this representation, the polynomial `3 + 5x + x^2` would be written as
`P [3, 5, 1]`. Make sure you fully understand this representation before moving
on to the exercises!

**Note**: You should use functions from `Data.List` as appropriate.

## Exercise 1

In this exercise, you will write an instance of `Eq` for `Poly`. The
automatically derived implementation would _not_ work:

```Haskell
instance Eq Poly where
  (==) :: Poly -> Poly -> Bool
  (P c1) == (P c2) = c1 == c2
```

There are situations where two polynomials are equivalent, but their list
representations are not. In particular, consider `P [1, 2, 0]` versus
`P [1, 2]`.

Implement the `(==)` function in a way that takes into account this subtlety
with zero coefficients. You will need to first erase the `deriving (Eq)`
underneath the definition of `Poly` and then implement your own `instance`.

## Exercise 2

In this exercise, you will write an instance of `Show` for `Poly`. For example,
`P [1, 2, 3]` should be displayed as `1 + 2x + 3x^2`.

It should satisfy these constraints:

-   Terms are displayed as `cx^e` where `c` is the coefficient and `e` is the
    exponent. If `e` is 0, then only the coefficient is displayed. If `e` is 1,
    then the format is simply `cx`.

    (Otherwise, we would have e.g. `1x^0 + 2x^1 + 3x^2`, which is uglier.)

-   Terms are separated by the `+` sign with a single space on each side.

-   As a special case, for `P []`, display `0` rather than the empty string.

We break this problem down into several steps.

1.  Fill in case for `P []`.
2.  Implement the helper function `showTerm`, which given a coefficient (the
    first agument) and an exponent (the second argument), generates the string
    for that term. For example, `showTerm 3 2` should evaluate to `"3x^2"`.
3.  Use `showTerm` to combine the list of coefficients `cs` and the list of
    degrees. Use `zipWith` to help with this. In the above example, you should
    get the list `["1", "2x", "3x^2"]`.
4.  Make the list into the final string by inserting `" + "` between each
    element. Find a list library function to help with this. It's a bit tricky
    to find — you may need to recall that a `String` is just a `[Char]`. If you
    get stuck, look in `hints.md`.

## Exercise 3

What is a number? In Haskell, a number is any type that is an instance of the
`Num` type class. Polynomials can be added, multiplied, and so on, just like any
other number. We will write an instance of `Num` for `Poly` over the course of
the remaining exercises.

First, implement `negate`, which should negate all of the coefficients of the
polynomial. For example, `P [1, 2, 3]` becomes `P [-1, -2, -3]`.

Second, implement `fromInteger`, which converts an integer into a degree zero
polynomial. For example, `3` becomes `P [3]`.

Remember that `negate` and `fromInteger` are functions that you can use on
any type that is an instance of `Num` type class, including for the `Int`
coefficients in the list.

## Exercise 4 (Optional)

Next, we define polynomial addition. We need to add the coefficients pairwise
for each term in the two polynomials.

For example, `(5 + x) + (1 + x + 3x^2) = 6 + 2x + 3x^2`. In terms of our
representation, this means that `P [5, 1] + P [1, 1, 3] = P [6, 2, 3]`.

If you would like a hint on this problem, please refer to `hints.md`.

## Exercise 5 (Optional)

Finally, we define polynomial multiplication. To multiply two polynomials, each
term in the first polynomial must be multiplied by each term in the second
polynomial. The easiest way to achieve this is to build up a `[Poly]` where each
element is the polynomial resulting from multiplying a single coefficient in the
first polynomial by each coefficient in the second polynomial.

Since the terms do not explicitly state their exponents, you will have to shift
the output before multiplying it by each consecutive coefficient. For example
`P [1, 1, 1] * P [2, 2]` will yield the list
`[P [2, 2], P [0, 2, 2], P [0, 0, 2, 2]]`. You can then simply sum this list,
since we've already defined `(+)`.

If you would like a hint on this problem, please refer to `hints.md`.

## Conclusion

Now that we've finished implementing the `Num` instance, we can write and
manipulate polynomials very similarly to how we would in mathematical notation.
See `final` for an example. Note that though we did not implement `^` and `-`
explicitly, these are automatically defined for us in terms of the functions we
did define.

## Grading

Please see Gradescope for the points breakdown.