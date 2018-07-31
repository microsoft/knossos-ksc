# Reverse mode with function definitions

-- third party is first party

This document explains how to perform reverse mode AD on a program
containing top level function declarations.

The source language contains primitive arithmetic operations and top
level function declarations.  The target language contains those plus
ADTs.  Each input declaration is translated to an output declaration.
If the input declaration has type `(Float, ..., Float) -> Float` then
the output declaration will have type `(Float, ..., Float) -> T`
for some target language ADT `T` which represents the trace of
execution of the program.

If we were writing `T` in Haskell it would look like this

```haskell
data T = T { val :: Float, tree :: Tree }

data Tree = Const Float
          | Var Symbol
          | PrimOp1 T ... T
          | ...
          | Let Symbol T T
```

Running the target program produces the trace.  Backpropagation is
performed on the trace to yield the derivative.

## Translating to a form which writes the trace

The translation rule `[.]` from the source language to the target
language is

```
[ decl1; ...; decln ] = [decl1]; ...; [decln]

[def f(x1, ..., xn) = e] = def f(x1, ..., xn) = [e]

[k] = T { val = k, tree = Const k }

-- The x in "Var x" is symbolic, not the value of the variable x
[x] = T { val = x, tree = Var x }

[primOp e1 ... en] = let te1 = [e1]
                         ...
                         ten = [en]
                     in (primOp (val te1) ... (val ten), PrimOp te1 ... ten)

[let y = e1 in e2] = let ty = [e1]
                         y  = val ty
                         tl = [e2]
                     in (val tl, Let y ty tl)

[f(e1, ..., en)] = [let x1 = e1
                        ...
                        xn = en
                    in f(x1, ..., xn)]
```

## Backpropagation on the trace

The result of translating the program to this form is a new program
which calculates the same result as the original program but also
returns the trace of execution.  From the trace of execution we can
backpropagate and calculate the derivative of the original program.
In Haskell backpropogation would be written as follows

```haskell
backpropagate :: Tree -> Float -> Map Symbol Float -> Map Symbol Float
backpropagate t ddr =
  case t of
    Const _ -> id
    Var x   -> adjust x (+ ddr)
    PrimOp+ t1 t2 ->
      backpropagate (tree t1) ddr
      . backpropagate (tree t2) ddr
    PrimOp- t1 t2 ->
      backpropagate (tree t1) ddr
      . backpropagate (tree t2) (-ddr)
    PrimOp* t1 t2 ->
      backpropagate (tree t1) (val t2 * ddr)
      . backpropagate (tree t2) (val t1 * ddr)
    PrimOp/ t1 t2 ->
      backpropagate (tree t1) ((1 / val t2) * ddr)
      . backpropagate (tree t2) (-val t1 / (val t2 * val t2) * ddr)
    ...
    Let y e1 e2 -> \m ->
      let m2  = backpropagate (tree t2) ddr m
          ddy = lookup y m2
      in backpropagate (tree t1) ddy (delete y m2)
```
