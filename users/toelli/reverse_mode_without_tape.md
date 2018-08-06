# Reverse mode without tape

A version of reverse mode AD without a tape.  Theoretically it's
asymptotically slower than reverse mode AD with a tape but maybe in
practice it's equally fast or faster.  It certainly uses less space!

We transform an expression

```
e :: R^m
```

with free variables `x1 :: t1, ..., xn :: tn` into an expression

```
[e] :: { x1 :: t1, ..., xn :: tn }
```

(where the `xi` are used as field labels) with free variables `x1 ::
t1, ..., xn :: tn, d_dr :: R^m` using the following rules

```
[k] = {}

[xi] = { xi = d_dr }

[v1 + v2] = { v1 = d_dr, v2 = d_dr }

[v1 * v2] = { v1 = v2 * d_dr, v2 = v1 * d_dr }

[(v1, ..., vn)] = { v1 = d_dr[1], ..., vn = d_dr[n] }

[f(v1, ..., vn)] = f`(v1, ..., vn; d_dr)

[f(v1, ..., vn) = e] = f(v1, ..., vn) = e
                       f`(v1, ..., vn, d_dr) = [e]

[let y = e0 in e1] = let y = e0
                         { y = y` } : rest = [e1]
                     in rest + let d_dr = y` in [e0]
```

## Complexity

The `let` rule runs `e0` and then `[e0]`.  In the worst case `[e0]`
has the same time complexity as `e0`.  This means that running reverse
mode without tape has runtime quadratic in the runtime of the original
program.

An example of a bad program under this transformation

```
let fa n x =
    if n == 0
    then 1
    else let y = fa (n-1) x
         in x * y
```

generates

```
let fa n x =
    if n == 0
    then 1
    else let y = fa (n-1) x
         in x * y

let fa` n x d_dr =
    if n == 0
    then 0
    else let y = fa (n-1) x
             d_dy = x * d_dr
             d_dx = y * d_dr
         in d_dx + fa` (n-1) x d_dy
```

The run time of the derivative is quadratic in `n`.
