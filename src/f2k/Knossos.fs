// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
/// These are stubs that we expect Knossos to substitute later, they may want to be given implementations
/// so that debugging on the F# side is possible.
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
let inline mul (a: Vector<Vector<float>>) (b: Vector<float>) : Vector<float> = 
    sum(build (size a) (fun i -> a.[i] * b.[i]))

let inline max (a: Vector<float>) = a.GetMaxBy( fun x->x )
let inline expv (a: Vector<float>) = Vector.map exp a
//let inline sqnorm (a: Vector<'a>) = a.GetL2NormSq()
let inline sqnorm (a: Vector<float>) = a.GetL2NormSq()
let gammaLn (a: float) = a

let rng = new System.Random()
let rand m = build m (fun _ -> rng.NextDouble())
let rand2 m n = build2 m n (fun i j -> rng.NextDouble())


let Float (x:float) = x

let Rows (A:Mat) = A.Length

let Cols (A:Mat) = A.[0].Length

let MatToArray (M:Mat) = 
    match M with  
    | Vector V -> V |> Array.map(fun v -> match v with | Vector x -> x | ZeroVector _ -> failwith "unsupported") |> Array.concat
    | ZeroVector _ -> failwith "unsupported"

let VecToArray (V:Vec) = Vector.toArray V    

let MatOfArray n (X:float[]) : Mat= 
    X 
    |> Array.chunkBySize (X.Length/n)
    |> Array.map (fun x -> Vector x)
    |> Vector    

let VecOfArray (X:float[]) : Vec = Vector X
    
let MatToVec (A:Mat) = Vector (MatToArray A)    

let VecToMat n (V:Vec) :Mat = 
    let V' = V |> VecToArray 
    V' |> Array.chunkBySize (V'.Length/n) |> Array.map Vector |> Vector

let VecConcat (X:seq<Vec>) = X |> Seq.map VecToArray |> Seq.concat |> Array.ofSeq |> Vector

let CreateRows n (x:Vec) : Mat = Array.replicate n x |> Vector

let reLU (A:Mat) = Vector.map (Vector.map (fun x -> System.Math.Max(x, 0.0))) A

let GetRows (A:Mat) = 
    match A with  
    | Vector V -> V 
    | ZeroVector _ -> failwith "unsupported"

let MatMul (A:Mat) (B:Mat) = build2 (Rows A) (Cols B) (fun i j -> sum(build (Cols A) (fun k -> A.[i].[k] * B.[k].[j])))