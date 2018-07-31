Why we need to compile top level functions to functions
===

-- i.e. not inline them

Here is an example of a concrete real-world program where we cannot
calculate the derivative by first inlining function calls to reduce
the program to an arithmetic expression.  The reason is that it
involves a fold which calls the accumulation function a number of
times that is not statically known.

> module Example where

Our deep learning framework comes with the following basic functions.

> data Mat
> data Vec
>
> vecRelu :: Vec -> Vec
> vecRelu = undefined
>
> (.+) :: Vec -> Vec -> Vec
> (.+) = undefined
>
> (.-) :: Vec -> Vec -> Vec
> (.-) = undefined
>
> (.*) :: Mat -> Vec -> Vec
> (.*) = undefined
>
> l2Norm :: Vec -> Float
> l2Norm = undefined
>
> someInitialVec :: Vec
> someInitialVec = undefined

We can write an RNN like this

> cell :: Mat -> Mat -> Vec -> Vec -> Vec -> Vec
> cell m1 m2 b prev word = vecRelu ((m1 .* prev) .+ (m2 .* word) .+ b)
>
> rnn :: [Vec] -> Mat -> Mat -> Vec -> Vec
> rnn words m1 m2 b = foldl (cell m1 m2 b) initialVec words
>     where initialVec = someInitialVec
>
> l2Distance :: Vec -> Vec -> Float
> l2Distance v1 v2 = l2Norm (v2 .- v1)
>
> loss :: [Vec] -> Vec -> Mat -> Mat -> Vec -> Float
> loss words objective m1 m2 b = l2Distance objective (rnn words m1 m2 b)

We want to calculate the derivative of `loss` with respect to the
`m1`, `m2` and `b` arguments (whilst "holding `cs` and `objective`
constant"), that is, we want to know the direction in which to perturb
the weights `m1`, `m2` and `b` in order to most reduce the loss.

This is a concrete real-world example of a program where we cannot
calculate the derivative by first inlining function calls to reduce
the program to an arithmetic expression. There is no way to inline
through the `foldl` call, since `foldl` is a recursive function.
Instead we need to directly compile `cell` to a function which
independently calculates the derivative of the original.
