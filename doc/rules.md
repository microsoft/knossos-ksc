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
```
