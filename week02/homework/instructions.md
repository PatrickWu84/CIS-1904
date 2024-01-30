# Homework 2: Algebraic Data Types

**Due**: Monday, Feb. 5 at 10 p.m.

In this homework, we will work with two mini-languages for conducting arithmetic.

## Exercise 1

We represent natural numbers as a data type with two constructors:

```Haskell
data Nat
  = Z
  | S Nat
```

The `Z` constructor represents the number `0`. The `S` constructor stands for
"successor," and `S n` represents the number that follows `n`: that is, `1 + n`.
For example, we would write `1` as `S Z` and `2` as `S (S Z)`.

Fill in the definition for addition. As a hint, our solution has just two cases:
one for when the first argument is zero, and one for when it is non-zero.

## Exercise 2

Our language for arithmetic expressions is defined as follows:

```Haskell
data Exp
  = Num Nat
  | Add Exp Exp
  | Branch Exp Exp Exp
```

Given an arbitrary `Exp`, we want to evaluate it to a `Nat`. In particular,

-   `Num n` should evaluate to `n`.

-   `Add e1 e2` should evaluate to the sum of the evaluation of `e1` and `e2`.

-   For `Branch e1 e2 e3`, there are two cases.

    If `e1` evaluates to zero, then we should take the first branch, so the
    expression as a whole should evaluate to the same thing as `e2`.

    And if `e1` is non-zero, we should take the second branch.

Fill in the `eval` function according to the above specification.

## Exercise 3

Our second language involves these instructions:

```Haskell
data Instr
  = IPush Nat
  | IAdd
  | IBranch
```

We also have _stacks_, which we represent as lists of natural numbers. Here, we
use a _type synonym_ to give a name to this type. The purpose is just
documentation! You can use a `Stack` exactly as you would a `[Nat]`.

```Haskell
type Stack = [Nat]
```

Instructions manipulate the stack. In particular,

- `IPush n` says to push the number `n` onto the stack.

- `IAdd` says to pop the first two numbers off the stack, add them, and push the
  result onto the stack.

- `IBranch` says to pop the first three numbers `n1`, `n2`, `n3` off the stack,
  If `n1` is zero, push `n2` onto the stack; otherwise, push `n3`.

If an instruction cannot be executed (i.e., the stack does not have enough
numbers), we simply ignore it and return the stack untouched.

Fill in the `exec1` function to execute a single instruction according to the
above specification.

## Exercise 4

Using the previously defined `exec1`, fill in the `exec` function to execute a
list of instructions. The output stack of the previous instruction should be
passed as the input stack of the next instruction.

## Exercise 5

**This problem is optional.** However, I recommend that you at least read it to
see how the previous exercises tie together!

We can compile (that is, translate) the expression language into the stack
language. The compilation is correct if it satisfies this property for any
expression `e`:

```Haskell
exec (compile e) [] = [eval e]
```

Take a few moments to make sure you understand this property, which says that
the result of evaluating an expression should be the same as the result of
executing the list of instructions that the expression compiles to.

For example, we should have

```Haskell
exec (compile (Add (Add (Num one) (Num two)) (Num three))) [] = [six]
```

Fill in the `compile` function to translate `Exp`s into lists of `Instr`s.

## Grading

Please see Gradescope for the points breakdown. As before, the autograder tests
will be different from the ones provided here. For the style points, I will pay
particular attention to your use of pattern matching.

## Source

Aspects of this assignment take inspiration from _Software Foundations_.