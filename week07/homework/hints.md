# Hints

## Exercise 2

The function you want for the last part is `intercalate`.

## Exercise 4

Consider this partial solution:

```Haskell
plus (P c1s) (P c2s) = P (zipWith (+) c1s c2s)
```

This does not quite work because `zipWith` ignores the extra elements if the
input lists are not the same length. There are many approaches to fixing this
problem; one is to pad the shorter list with zeroes.

## Exercise 5

Consider this partial solution:

```Haskell
times :: Poly -> Poly -> Poly
times (P c1) (P c2) = sum (go c1 c2)
  where
    go :: [Int] -> [Int] -> [Poly]
    go [] _ = _
    go (c1 : c1s) c2s = _
```

As described in the instructions, we want to multiply each coefficient in the
first polynomial by each coefficient in the second.

In the base case of `go`, consider what `0 *` anything should be. In the
recursive case, use `map` to multiply every coefficient in `c2s` by `c1`, and
then in the recursive call, shift `c2s` by adding a `0` to the front.
