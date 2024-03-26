# Homework 7: IO

**Due**: Monday, April 1 at 10 p.m.

## Exercise 1

Take a look
[here](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#g:29).
Observe that `FilePath` is just a _type synonym_ for `String` — they're entirely
interchangeable, but for documentation purposes, it's nice to have a new name.

Try using `readFile` and `writeFile` a bit. Figure out if the following
statements are true or false, and fill in the corresponding variable.

1. `readFile` will error out if given a file name that does not exist.

2. `writeFile` will error out if given a file name that does not exist.

3. `writeFile` will overwrite the contents of an existing file.

## Exercise 2

Practice using `readFile` and `writeFile` by implementing the short functions
below. Do **not** use any recursive functions in your solution.

### (a)

`lengthFile` should return the number of characters (including whitespace)
in the contents of the input file.

### (b)

`concatFiles` takes as parameters a list of input files and an output file. It
should concatenate the contents of the input files and write those contents into
the output file.

You should use `mapM`, which for our purposes has type
`(a -> IO b) -> [a] -> IO [b]`.

## Exercise 3

In this exercise, you will reimplement some `IO` functions for printing.
Note that `putChar :: Char -> IO ()` takes a character and prints it.

1. (Re)implement `putStr` in terms of `putChar`.
   Do it twice: first with a recursive function, then with `mapM_`, which for
   our purposes has type `(a -> IO b) -> [a] -> IO ()`.

2. (Re)implement `putStrLn` in terms of `putStr'`.

3. (Re)implement `print` in terms of `putStrLn'`.

If you are not sure how some of these functions work, refer to the documentation
[here](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#g:27)
or try them out in GHCi.

To test your reimplementations, make sure this script works in GHCi:

```
ghci> putStr' "hello, world!"
hello, world!ghci> putStrLn' "hello again"
hello again
ghci> print' 3
3
```

## Grading

Please see Gradescope for the points breakdown. Exercises 1 and 2 will be
autograded; Exercise 3 will be manually graded. I will also look to see if your
solutions followed the constraints and made effective use of `do` notation.