# Some useful rewrite rules

A collection of useful rewrite ruless.  This document will probably be
written up into a better LaTeX document at some point in the future.

In the following, identifiers stand for expressions that may have free
variables.

```
sum (deltaVec n i e) = e

index ki (build k (lam ki e)) = e

sumbuild o (lam oi (build k (lam ki e)))
  = build k (lam ki (sumbuild o (lam oi e)))

index ki (sumbuild o (lam oi e))
  = sumbuild o (lam oi (index ki e))

index ki (sum e)
  = sum (build (size e) (lam i (index ki (index i e))))

sum (build k (lam ki e))
  = sumbuild k (lam ki e)

sumbuild k (lam ki (deltaVec o i e))
  = deltaVec o i (sumbuild k (lam ki e))
# ki should not appear free in o or i

sumbuild k (lam ki (deltaVec k ki e))
  = build k (lam ki e)

sumbuild k (lam ki (if c t f))
  = if c (sumbuild k (lam ki t)) (sumbuild k (lam ki f))
# ki should not appear free in c

build k (lam ki (if c t f))
  = if c (build k (lam ki t)) (build k (lam ki f))
# ki should not appear free in c

op (if c t f)
  = if c (op t) (op f)
```
