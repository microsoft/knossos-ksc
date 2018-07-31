# Worked example of autodiff

(An elucidation of [an earlier example by Andrew
Fitzgibbon](https://notebooks.azure.com/anon-4ufduw/libraries/fsad/html/F%23%20Autodiff%20experiments.ipynb))

## Reverse mode

Suppose we have the program

```
p = 7 * x
r = 1 / y
q = p * x * 5
v = 2 * p * q + 3 * r
```

(which has free variables `x` and `y`) and we want to calculate `dv/dx` and
`dv/dy`.

* Step 1: Write down what you want to calculate

We want to calculate `dα/dx` and `dα/dy`, where `α` is a placeholder
that we will set to `v` in the final reckoning.  By the chain rule

```
dα/dx = dα/dp * dp/dx + dα/dq * dq/dx + dα/dr * dr/dx + dα/dv * dv/dx
dα/dy = dα/dp * dp/dy + dα/dq * dq/dy + dα/dr * dr/dy + dα/dv * dv/dy
```

* Step 2: Recursively expand the new terms that have been introduced

Let's recapitulate the chain rule and follow it with the calculation of the
new terms introduced until we have calculated everything needed.

```
dα/dx = dα/dp * dp/dx + dα/dq * dq/dx + dα/dr * dr/dx + dα/dv * dv/dx
dα/dy = dα/dp * dp/dy + dα/dq * dq/dy + dα/dr * dr/dy + dα/dv * dv/dy

dp/dx = 7
dp/dy = 0

dq/dx = p * 5
dq/dy = 0

dr/dx = 0
dr/dy = -1/(y*y)

dv/dx = 0
dv/dy = 0

dα/dp = dα/dr * dr/dp + dα/dq * dq/dp + dα/dv * dv/dp

dr/dp = 0
dq/dp = x * 5
dv/dp = 2 * q

dα/dr = dα/dq * dq/dr + dα/dv * dv/dr

dq/dr = 0
dv/dr = 3

dα/dq = dα/dv * dv/dq

dv/dq = 2 * p
```

If we reverse these lines and put them after the original program then
we have a new (imperative) program which calculates the derivatives
`dα/dx` and `dα/dy` by reverse mode AD.  To calculate `dv/dx` and
`dv/dy` we set `dα/dv` to 1.

```
dα/dv = 1

p = 7 * x
r = 1 / y
q = p * x * 5
v = 2 * p * q + 3 * r

dv/dq = 2 * p

dα/dq = dα/dv * dv/dq

dq/dr = 0
dv/dr = 3

dα/dr = dα/dq * dq/dr + dα/dv * dv/dr

dr/dp = 0
dq/dp = x * 5
dv/dp = 2 * q

dα/dp = dα/dr * dr/dp + dα/dq * dq/dp + dα/dv * dv/dp

dv/dx = 0
dv/dy = 0

dr/dx = 0
dr/dy = -1/(y*y)

dq/dx = p * 5
dq/dy = 0

dp/dx = 7
dp/dy = 0

dα/dx = dα/dp * dp/dx + dα/dq * dq/dx + dα/dr * dr/dx + dα/dv * dv/dx
dα/dy = dα/dp * dp/dy + dα/dq * dq/dy + dα/dr * dr/dy + dα/dv * dv/dy
```

## Forward mode

* Step 1: Write down what you want to calculate

Forward mode will let us calculate `dv/dα` where `α` is one of `x` and
`y`.  By the usual chain rule

```
dv/dα = dv/dp * dp/dα + dv/dq * dq/dα + dv/dr * dr/dα
```

* Step 2: Recursively expand the new terms that have been introduced

```
dv/dα = dv/dp * dp/dα + dv/dq * dq/dα + dv/dr * dr/dα

dv/dp = 2 * q
dv/dq = 2 * p
dv/dr = 3

dq/dα = dq/dp * dp/dα + dq/dx * dx/dα

dq/dp = x * 5
dq/dx = p * 5

dr/dα = -1 / (y * y) * dy/dα

dp/dα = dp/dx * dx/dα

dp/dx = 7
```

Again, if we reverse these lines and put them after the original program
then we have a new (imperative) program which calculates the derivative
`dv/dα`, this time by forward mode AD.  To calculate `dv/dx`, for
example, we set `dx/dα` to 1 and `dy/dα` to 0.

```
dx/dα = 1
dy/dα = 0

p = 7 * x
r = 1 / y
q = p * x * 5
v = 2 * p * q + 3 * r

dp/dx = 7

dp/dα = dp/dx * dx/dα

dr/dα = -1 / (y * y) * dy/dα

dq/dp = x * 5
dq/dx = p * 5

dq/dα = dq/dp * dp/dα + dq/dx * dx/dα

dv/dp = 2 * q
dv/dq = 2 * p
dv/dr = 3

dv/dα = dv/dp * dp/dα + dv/dq * dq/dα + dv/dr * dr/dα
```

# Functional form

## Forward  mode

```
Γ, x1, ..., xn |- e
-----------------------------------------------
Γ, x1, ..., xn, dx1, ..., dxn |- D_x1,...,xn[e]


Γ, f, x1, ..., xn |- f x1 ... xn
------------------------------------------------------------
Γ, f, x1, ..., xn, dx1, ..., dxn |- df x1 ... xn dx1 ... dxn

D_x1,...,xn[xi] = dxi (if xi occurs amongst the x1, ..., xn)
D_...[v] = 0 (otherwise)
D_...[k] = 0

D_...[let x = e in e'] = let y = e; dx = D_...[e] in D_x,...[e']

D...[let f x1 ... xn = e in e']
    = let f x1 ... xn = e
          df x1 ... xn dx1 ... dxn = D...[e]
      in D...[e']

D...  [f x1 ... xn] = df x1 ... xn dx1 ... dxn

d+ x1 x2 dx1 dx2 = dx1 + dx2 (or, d+ _ _ = (+))
d* x1 x2 dx1 dx2 = x2 * dx1 + x1 * dx2
dfst (x1, x2) (dx1, dx2) = dx1 (or, dfst _ = fst)
```
