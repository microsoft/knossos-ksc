// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
/// These are stubs that we expect Knossos to substitute later, 
/// they should given implementations so that debugging on the F# side 
/// produces the same results as compilation 
module Knossos

open DV
type Vec = Vector<float>
type Mat = Vector<Vector<float>>

let invSqrtGaussian_sample _ _ _ = 0.0
type RNG =
    class end
let Q = id
let categorical_sample _ _ : int = 0


let inline size (v: Vector<'T>) = v.Length

let inline build n (f : int -> 'T) = Vector.init n f
let inline build2 m n (f : int -> int -> float) = Vector.init m (fun i -> Vector.init n (f i))
let inline sum v = Vector.sum v
let inline mvmul (a: Vector<Vector<float>>) (b: Vector<float>) : Vector<float> = 
    sum(build (size a) (fun i -> a.[i] * b.[i]))

let inline map (f: 'a -> 'b) (x: Vector<'a>) : Vector<'b> = 
    build (size x) (fun i -> f x.[i])

let inline max (a: Vector<float>) = a.GetMaxBy( fun x->x )
let inline expv (a: Vector<float>) = Vector.map exp a
//let inline sqnorm (a: Vector<'a>) = a.GetL2NormSq()
let inline sqnorm (a: Vector<float>) = a.GetL2NormSq()
let gammaLn (a: float) = a

let rng = new System.Random()
let rand m = build m (fun _ -> rng.NextDouble())
let rand2 m n = build2 m n (fun i j -> rng.NextDouble())
// Square a number
let inline sqr x = x * x

