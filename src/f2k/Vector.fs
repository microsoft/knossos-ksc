//
// This file is part of
// FsAlg: Generic Linear Algebra Library
//
// Copyright (c) 2015, National University of Ireland Maynooth (Atilim Gunes Baydin, Barak A. Pearlmutter)
//
// FsAlg is released under the BSD license.
// (See accompanying LICENSE file.)
//
// Written by:
//
//   Atilim Gunes Baydin
//   atilimgunes.baydin@nuim.ie
//
//   Barak A. Pearlmutter
//   barak@cs.nuim.ie
//
//   Brain and Computation Lab
//   Hamilton Institute & Department of Computer Science
//   National University of Ireland Maynooth
//   Maynooth, Co. Kildare
//   Ireland
//
//   www.bcl.hamilton.ie
//

namespace DV

open Util

/// Generic vector type
[<NoEquality; NoComparison>]
type Vector<'T when 'T : (static member Zero : 'T)
                and 'T : (static member One : 'T)
                and 'T : (static member (+) : 'T * 'T -> 'T)
                and 'T : (static member (-) : 'T * 'T -> 'T)
                and 'T : (static member (*) : 'T * 'T -> 'T)
//                and 'T : (static member (/) : 'T * 'T -> 'T)
                and 'T : (static member (~-) : 'T -> 'T)
                and 'T : (static member Abs : 'T -> 'T)
                and 'T : (static member Pow : 'T * float -> 'T)
                and 'T : (static member Sqrt : 'T -> 'T)
//                and 'T : (static member op_Explicit : 'T -> float)
//                and 'T : comparison
                > =
    | ZeroVector of 'T
    | Vector of 'T[]
    /// ZeroVector
    static member inline Zero = ZeroVector LanguagePrimitives.GenericZero<'T>
    static member inline One = Vector([|LanguagePrimitives.GenericOne<'T>|])
(*    /// Converts vector `v` to T[]
    static member inline op_Explicit(v:Vector<'T>) : 'T[] =
        match v with
        | Vector av -> Array.map (fun x -> x :> 'T) av
        | ZeroVector _ -> [||] *)
    /// The element of this vector at the given position `i`
    member inline v.Item
        with get i =
            match v with
            | Vector v -> v.[i]
            | ZeroVector z -> z
        and set i vv =
            match v with
            | Vector v -> v.[i] <- vv
            | ZeroVector _ -> ()
    /// Gets a subvector between bounds `lower` and `upper`
    member inline v.GetSlice(lower, upper) =
        match v with
        | Vector v ->
            let l = defaultArg lower 0
            let u = defaultArg upper (v.Length - 1)
            if l > u then invalidArg "" "Given slice bounds are invalid."
            Vector v.[l..u]
        | ZeroVector _ -> invalidArg "" "Cannot get slice of a ZeroVector."
    /// Gets the first element of this vector
    member inline v.FirstItem =
        match v with
        | Vector v -> v.[0]
        | ZeroVector z -> z
    /// Gets the total number of elements of this vector
    member inline v.Length =
        match v with
        | Vector v -> v.Length
        | ZeroVector _ -> 0
(*    /// Gets the L1 (Manhattan) norm of this vector
    member inline v.GetL1Norm() =
        match v with
        | Vector v -> Array.sumBy abs v
        | ZeroVector z -> z
*)
    /// Gets the L2 (Euclidean) norm of this vector
    member inline v.GetL2Norm() =
        match v with
        | Vector v -> sqrt (Array.sumBy (fun x -> x * x) v)
        | ZeroVector z -> z
    /// Gets the squared L2 (Euclidean) norm of this vector
    member inline v.GetL2NormSq() =
        match v with
        | Vector v -> Array.sumBy (fun x -> x * x) v
        | ZeroVector z -> z
    /// Gets the Lp norm (or p-norm) of this vector, with the given `p`

(*    member inline v.GetLPNorm(p:float):float =
        match v with
        | Vector v -> (Array.sumBy (fun x -> (abs x) ** p) v) ** (LanguagePrimitives.GenericOne<'T> / p)
        | ZeroVector z -> z *)
(*    /// Gets the minimum element of this vector
    member inline v.GetMin() =
        match v with
        | Vector v -> Array.min v
        | ZeroVector z -> z *)
    /// Gets the minimum element of this vector, compared by using Operators.min on the result of function `f`
    member inline v.GetMinBy(f) =
        match v with
        | Vector v -> Array.minBy f v
        | ZeroVector z -> z
(*    /// Gets the maximum element of this vector
    member inline v.GetMax() =
        match v with
        | Vector v -> Array.max v
        | ZeroVector z -> z *)
    /// Gets the maximum element of this vector, compared by using Operators.max on the result of function `f`
    member inline v.GetMaxBy(f) =
        match v with
        | Vector v -> Array.maxBy f v
        | ZeroVector z -> z
(*    /// Gets the unit vector codirectional with this vector
    member inline v.GetUnitVector() =
        match v with
        | Vector vv -> let n = v.GetL2Norm() in Vector (Array.map (fun x -> x / n) vv)
        | ZeroVector z -> ZeroVector z *)
    /// Returns a sequence of vectors that are obtained by splitting this vector into subvectors whose lengths are given in sequence `n`
    member inline v.Split(n:seq<int>) =
        match v with
        | Vector v ->
            let i = ref 0
            seq {for j in n do
                    yield Array.sub v !i j |> Vector
                    i := !i + j}
        | ZeroVector _ -> seq {yield v}
    /// Returns a sequence of vectors that are obtained by splitting this vector into `n` subvectors of equal length. The length of this vector must be an integer multiple of `n`, otherwise ArgumentException is raised.
    member inline v.SplitEqual(n:int) =
        match v with
        | Vector v ->
            if n <= 0 then invalidArg "" "For splitting this vector, n should be a positive integer."
            let l = (float v.Length) / (float n)
            if not (isInteger l) then invalidArg "" "Cannot split vector into n equal pieces when length of vector is not an integer multiple of n."
            seq {for i in 0 .. (int l) .. (v.Length - 1) do yield Vector (Array.sub v i (int l))}
        | ZeroVector _ -> seq {yield v}
    /// Gets a string representation of this vector that can be pasted into a Mathematica notebook
    member inline v.ToMathematicaString() = 
        let sb = System.Text.StringBuilder()
        sb.Append("{") |> ignore
        for i = 0 to v.Length - 1 do
            sb.Append(sprintf "%A" v.[i]) |> ignore
            if i < v.Length - 1 then sb.Append(", ") |> ignore
        sb.Append("}") |> ignore
        sb.ToString()
    /// Gets a string representation of this vector that can be pasted into MATLAB
    member inline v.ToMatlabString() =
        let sb = System.Text.StringBuilder()
        sb.Append("[") |> ignore
        for i = 0 to v.Length - 1 do
            sb.Append(sprintf "%A" v.[i]) |> ignore
            if i < v.Length - 1 then sb.Append(" ") |> ignore
        sb.Append("]") |> ignore
        sb.ToString()
    /// Converts the elements of this vector to another type, using the given conversion function `f`
    member inline v.Convert(f:'T->'a):Vector<'a> =
        match v with
        | Vector v -> Vector (Array.map f v)
        | ZeroVector _ -> ZeroVector LanguagePrimitives.GenericZero<'a>
    /// Creates a copy of this vector
    member inline v.Copy() =
        match v with
        | Vector v -> Vector (Array.copy v)
        | ZeroVector z -> ZeroVector z
    /// Converts this vector to an array
    member inline v.ToArray() =
        match v with
        | Vector v -> v
        | ZeroVector _ -> [||]
    /// Converts this vector to a sequence
    member inline v.ToSeq() =
        match v with
        | Vector v -> Array.toSeq v
        | ZeroVector _ -> Seq.empty
    /// Creates a new vector that contains the given subrange of elements, specified by start index `s` and count `c`
    member inline v.GetSubVector(s, c) =
        match v with
        | Vector v -> Vector (Array.sub v s c)
        | ZeroVector _ -> Vector.Zero
    /// Returns an enumerator that iterates through the elements of this vector
    member inline v.GetEnumerator() =
        v.ToSeq().GetEnumerator()
    /// Adds vector `a` to vector `b`
    static member inline (+) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector a, Vector b -> try Vector (Array.map2 (+) a b) with | _ -> invalidArg "" "Cannot add two vectors of different dimensions."
        | Vector _, ZeroVector _ -> a
        | ZeroVector _, Vector _ -> b
        | ZeroVector _, ZeroVector _ -> Vector.Zero
    /// Subtracts vector `b` from vector `a`
    static member inline (-) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector a, Vector b -> try Vector (Array.map2 (-) a b) with | _ -> invalidArg "" "Cannot subtract two vectors of different dimensions."
        | Vector _, ZeroVector _ -> a
        | ZeroVector _, Vector b -> Vector (Array.map (~-) b)
        | ZeroVector _, ZeroVector _ -> Vector.Zero
    /// Computes the inner product (dot / scalar product) of vector `a` and vector `b`
    static member inline dot (a:Vector<'T>, b:Vector<'T>):'T =
        match a, b with
        | Vector a, Vector b -> try Array.map2 (*) a b |> Array.sum with | _ -> invalidArg "" "Cannot multiply two vectors of different dimensions."
        | Vector _, ZeroVector _ -> LanguagePrimitives.GenericZero<'T>
        | ZeroVector _, Vector _ -> LanguagePrimitives.GenericZero<'T>
        | ZeroVector _, ZeroVector _ -> LanguagePrimitives.GenericZero<'T>
(*    /// Computes the cross product of vector `a` and vector `b` (three-dimensional)
    static member inline (%* ) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector va, Vector vb ->
            if (a.Length <> 3) || (b.Length <> 3) then invalidArg "" "The cross product is only defined for three-dimensional vectors."
            Vector [|va.[1] * vb.[2] - va.[2] * vb.[1]; va.[2] * vb.[0] - va.[0] * vb.[2]; va.[0] * vb.[1] - va.[1] * vb.[0]|]
        | Vector _, ZeroVector _ -> Vector.Zero
        | ZeroVector _, Vector _ -> Vector.Zero
        | ZeroVector _, ZeroVector _ -> Vector.Zero
*)    /// Multiplies vector `a` and vector `b` element-wise (Hadamard product)
    static member inline (*) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector a, Vector b -> try Vector (Array.map2 (*) a b) with | _ -> invalidArg "" "Cannot multiply two vectors of different dimensions."
        | Vector _, ZeroVector _ -> Vector.Zero
        | ZeroVector _, Vector _ -> Vector.Zero
        | ZeroVector _, ZeroVector _ -> Vector.Zero

        (*
    /// Divides vector `a` by vector `b` element-wise (Hadamard division)
    static member inline (./) (a:Vector<'T>, b:Vector<'T>):Vector<'T> =
        match a, b with
        | Vector a, Vector b -> try Vector (Array.map2 (/) a b) with | _ -> invalidArg "" "Cannot divide two vectors of different dimensions."
        | Vector _, ZeroVector _-> raise (new System.DivideByZeroException("Attempted to divide a Vector by a ZeroVector."))
        | ZeroVector _, Vector _ -> Vector.Zero
        | ZeroVector _, ZeroVector _ -> raise (new System.DivideByZeroException("Attempted to divide a ZeroVector by a ZeroVector."))
        *)
    /// Adds scalar `b` to each element of vector `a`
    static member inline (+) (a:Vector<'T>, b:'T):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map ((+) b) a)
        | ZeroVector _ -> invalidArg "" "Unsupported operation. Cannot add a scalar to a ZeroVector."
    /// Adds scalar `a` to each element of vector `b`
    static member inline (+) (a:'T, b:Vector<'T>):Vector<'T> =
        match b with
        | Vector b -> Vector (Array.map ((+) a) b)
        | ZeroVector _ -> invalidArg "" "Unsupported operation. Cannot add a scalar to a ZeroVector."
    /// Subtracts scalar `b` from each element of vector `a`
    static member inline (-) (a:Vector<'T>, b:'T):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map (fun x -> x - b) a)
        | ZeroVector _ -> invalidArg "" "Unsupported operation. Cannot subtract a scalar from a ZeroVector."
    /// Subtracts each element of vector `b` from scalar `a`
    static member inline (-) (a:'T, b:Vector<'T>):Vector<'T> =
        match b with
        | Vector b -> Vector (Array.map ((-) a) b)
        | ZeroVector _ -> invalidArg "" "Unsupported operation. Cannot add subtract a ZeroVector from a scalar."
    /// Multiplies each element of vector `a` by scalar `b`
    static member inline (*) (a:Vector<'T>, b:'T):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map ((*) b) a)
        | ZeroVector _ -> Vector.Zero
    /// Multiplies each element of vector `b` by scalar `a`
    static member inline (*) (a:'T, b:Vector<'T>):Vector<'T> =
        match b with
        | Vector b -> Vector (Array.map ((*) a) b)
        | ZeroVector _ -> Vector.Zero
        (*
    /// Divides each element of vector `a` by scalar `b`
    static member inline (/) (a:Vector<'T>, b:'T):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map (fun x -> x / b) a)
        | ZeroVector _ -> Vector.Zero
    /// Divides scalar `a` by each element of vector `b`
    static member inline (/) (a:'T, b:Vector<'T>):Vector<'T> =
        match b with
        | Vector b -> Vector (Array.map ((/) a) b)
        | ZeroVector _ -> raise (new System.DivideByZeroException("Attempted division by a ZeroVector.")) 
        *)
    /// Gets the negative of Vector `a`
    static member inline (~-) (a:Vector<'T>):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map (~-) a)
        | ZeroVector _ -> Vector.Zero

     /// Gets the abs of Vector `a`
    static member inline Abs (a:Vector<'T>):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map abs a)
        | ZeroVector _ -> Vector.Zero

    /// Gets the sqrt of Vector `a`
    static member inline Sqrt (a:Vector<'T>):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map sqrt a)
        | ZeroVector _ -> Vector.Zero

    /// Gets the (pow _ n) of Vector `a`
    static member inline Pow (a:Vector<'T>, n:float):Vector<'T> =
        match a with
        | Vector a -> Vector (Array.map (fun t -> (t ** n)) a)
        | ZeroVector _ -> Vector.Zero

/// Operations on Vector type. (Implementing functionality similar to Microsoft.FSharp.Collections.Array)
module Vector =
    /// Createsa vector from array `v`
    let inline ofArray (v:'T[]) = Vector v
    /// Converts vector `v` to an array
    let inline toArray (v:Vector<'T>):'T[] = v.ToArray()
    /// Creates a vector from sequence `s`
    let inline ofSeq (s:seq<'T>):Vector<'T> = Vector (Array.ofSeq s)
    /// Returns vector `v` as a sequence
    let inline toSeq (v:Vector<'T>):seq<'T> = v.ToSeq()
    /// Creates a vector that contains the elements of vector `v1` followed by the elements of vector `v2`
    let inline append (v1:Vector<'T>) (v2:Vector<'T>):Vector<'T> = Array.append (v1 |> toArray) (v2 |> toArray) |> Vector
    /// Builds a new vector that contains the elements of each of the given sequence of vectors `v`
    let inline concat (v:seq<Vector<'T>>):Vector<'T> = Seq.map toArray v |> Array.concat |> ofSeq
    /// Creates a copy of vector `v`
    let inline copy (v:Vector<'T>):Vector<'T> = v.Copy()
    /// Creates a vector with `n` elements, all having value `v`
    let inline create (n:int) (v:'T):Vector<'T> = Vector (Array.create n v)
    /// Creates a vector with `n` elements, where the element with index `i` has value `v` and the rest of the elements have value 0
    let inline createBasis (n:int) (i:int) (v:'T):Vector<'T> = Vector (Array.init n (fun j -> if j = i then v else LanguagePrimitives.GenericZero))
    /// Tests if any element of vector `v` satisfies predicate `p`
    let inline exists (p:'T->bool) (v:Vector<'T>):bool = v |> toArray |> Array.exists p
    /// Tests if any pair of corresponding elements of vectors `v1` and `v2` satisfies predicate `p`
    let inline exists2 (p:'T1->'T2->bool) (v1:Vector<'T1>) (v2:Vector<'T2>):bool = Array.exists2 p (v1 |> toArray) (v2 |> toArray)
    /// Fills a range of elements of vector `v` with value `a` starting with index `s` and counting `c` elements
    let inline fill (v:Vector<'T>) (s:int) (c:int) (a:'T):unit = Array.fill (v |> toArray) s c a
    /// Returns the first element of vector `v` for which predicate `p` is true
    let inline find (p:'T->bool) (v:Vector<'T>):'T = v |> toArray |> Array.find p
    /// Returns the index of the first element of vector `v` for which predicate `p` is true
    let inline findIndex (p:'T->bool) (v:Vector<'T>):int = v |> toArray |> Array.findIndex p
    /// Applies function `f` to each element of vector `v`, threading an accumulator (with initial state `s`) through the computation. If the input function is f and the elements are i0...iN then computes f (... (f s i0)...) iN.
    let inline fold (f:'S->'T->'S) (s:'S) (v:Vector<'T>):'S = v |> toArray |> Array.fold f s
    /// Applies function `f` to corresponding elements of vectors `v1` and `v2`, threading an accumulator (with initial state `s`) through the computation. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let inline fold2 (f:'S->'T1->'T2->'S) (s:'S) (v1:Vector<'T1>) (v2:Vector<'T2>):'S = Array.fold2 f s (v1 |> toArray) (v2 |> toArray)
    /// Applies function `f` to each element of vector `v`, threading an accumulator (with initial state `s`) through the computation. If the input function is f and the elements are i0...iN then computes f i0 (...(f iN s)).
    let inline foldBack (f:'T->'S->'S) (v:Vector<'T>) (s:'S):'S = Array.foldBack f (v |> toArray) s
    /// Applies function `f` to corresponding elements of vectors `v1` and `v2`, threading an accumulator (with initial state `s`) through the computation. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let inline foldBack2 (f:'T1->'T2->'S->'S) (v1:Vector<'T1>) (v2:Vector<'T2>) (s:'S):'S = Array.foldBack2 f (v1 |> toArray) (v2 |> toArray) s
    /// Tests if all elements of vector `v` satisfy predicate `p`
    let inline forall (p:'T->bool) (v:Vector<'T>):bool = v |> toArray |> Array.forall p
    /// Tests if all corresponding elements of vectors `v1` and `v2` satisfy predicate `p` pairwise
    let inline forall2 (p:'T1->'T2->bool) (v1:Vector<'T1>) (v2:Vector<'T2>):bool = Array.forall2 p (v1 |> toArray) (v2 |> toArray)
    /// Gets the element of vector `v` with index `i`
    let inline get (v:Vector<'T>) (i:int):'T = v.[i]
    /// Creates a vector with dimension `n` and a generator function `f` to compute the elements
    let inline init (n:int) (f:int->'T):Vector<'T> = Vector (Array.init n f)
    /// Applies function `f` to each element of vector `v`
    let inline iter (f:'T->unit) (v:Vector<'T>):unit = v |> toArray |> Array.iter f
    /// Applies function `f` to corresponding elements of vectors `v1` and `v2` pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let inline iter2 (f:'T1->'T2->unit) (v1:Vector<'T1>) (v2:Vector<'T2>):unit = Array.iter2 f (v1 |> toArray) (v2 |> toArray)
    /// Applies function `f` to each element of vector `v`. The integer passed to function `f` indicates the index of element.
    let inline iteri (f:int->'T->unit) (v:Vector<'T>):unit = v |> toArray |> Array.iteri f
    /// Applies function `f` to corresponding elements of vectors `v1` and `v2` pairwise. The integer passed to function `f` indicates the index of element. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let inline iteri2 (f:int->'T1->'T2->unit) (v1:Vector<'T1>) (v2:Vector<'T2>):unit = Array.iteri2 f (v1 |> toArray) (v2 |> toArray)
(*
    /// Gets the L1 (Manhattan) norm of vector `v`
    let inline l1norm (v:Vector<'T>):'T = v.GetL1Norm()
    /// Gets the L2 (Euclidean) norm of vector `v`. This is the same with `Vector.norm`.
    let inline l2norm (v:Vector<'T>):'T = v.GetL2Norm()
    /// Gets the squared L2 (Euclidean) norm of vector `v`. This is the same with `Vector.normSq`.
    let inline l2normSq (v:Vector<'T>):'T = v.GetL2NormSq()
    /// Returns the length of vector `v`
    let inline length (v:Vector<'T>):int = v.Length
    /// Gets the Lp norm (or p-norm) of vector `v`, with the given `p`
//    let inline lpnorm (p:'T) (v:Vector<'T>):'T = v.GetLPNorm(p)
    /// Creates a vector whose elements are the results of applying function `f` to each element of vector `v`
*)
    let inline map (f:'T->'U) (v:Vector<'T>):Vector<'U> = v |> toArray |> Array.map f |> Vector
    /// Creates a vector whose elements are the results of applying function `f` to corresponding elements of vectors `v1` and `v2` pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
(*    let inline map2 (f:'T1->'T2->'U) (v1:Vector<'T1>) (v2:Vector<'T2>):Vector<'U> = Array.map2 f (v1 |> toArray) (v2 |> toArray) |> Vector
    /// Creates a vector whose elements are the results of applying function `f` to each element of vector `v`. An element index is also supplied to function `f`.
    let inline mapi (f:int->'T->'U) (v:Vector<'T>):Vector<'U> = v |> toArray |> Array.mapi f |> Vector
    /// Creates a vector whose elements are the results of applying function `f` to corresponding elements of vectors `v1` and `v2` pairwise. The integer passed to function `f` indicates the index of element. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let inline mapi2 (f:int->'T1->'T2->'U) (v1:Vector<'T1>) (v2:Vector<'T2>):Vector<'U> = Array.mapi2 f (v1 |> toArray) (v2 |> toArray) |> Vector
    /// Returns the maximum of all elements of vector `v`
    let inline max (v:Vector<'T>):'T = v.GetMax()
    /// Returns the maximum of all elements of vector `v`, compared by using Operators.max on the result of function `f`
    let inline maxBy (f:'T->'U) (v:Vector<'T>):'T = v.GetMaxBy(f)
    /// Returns the minimum of all elements of vector `v`
    let inline min (v:Vector<'T>):'T = v.GetMin()
    /// Returns the minimum of all elements of vector `v`, compared by using Operators.min on the result of function `f`
    let inline minBy (f:'T->'U) (v:Vector<'T>):'T = v.GetMinBy(f)
    /// Gets the L2 (Euclidean) norm of vector `v`. This is the same with `Vector.l2norm`.
    let inline norm (v:Vector<'T>):'T = l2norm v
    /// Gets the squared L2 (Euclidean) norm of vector `v`. This is the same with `Vector.l2normSq`.
    let inline normSq (v:Vector<'T>):'T = l2normSq v
    /// Applies function `f` to each element of vector `v`, threading an accumulator argument through the computation. If the input function is f and the elements are i0...iN, then computes f (... (f i0 i1)...) iN.
    let inline reduce (f:'T->'T->'T) (v:Vector<'T>):'T = v |> toArray |> Array.reduce f
    /// Applies function `f` to each element of vector `v`, threading an accumulator argument through the computation. If the input function is f and the elements are i0...iN then computes f i0 (...(f iN-1 iN)).
    let inline reduceBack (f:'T->'T->'T) (v:Vector<'T>):'T = v |> toArray |> Array.reduceBack f
    /// Replaces the elements of vector `v` by mutating them in place, passing them through function `f`
    let inline replace (f:'T->'T) (v:Vector<'T>):unit = for i in 0..(v.Length - 1) do v.[i] <- f v.[i]
    /// Replaces the elements of vector `v1` by mutating them in place. The new values are computed by applying function `f` to corresponding elements of vectors `v1` and `v2` pairwise. The two input vectors must have the same lengths, otherwie ArgumentException is raised.
    let inline replace2 (f:'T1->'T2->'T1) (v1:Vector<'T1>) (v2:Vector<'T2>):unit = 
        if v1.Length <> v2.Length then invalidArg "" "The vectors should have the same length."
        for i in 0..(v1.Length - 1) do v1.[i] <- f v1.[i] v2.[i]
    /// Replaces the elements of vector `v` by mutating them in place, passing them through function `f`. An element index is also supplied to function `f`.
    let inline replacei (f:int->'T->'T) (v:Vector<'T>):unit = for i in 0..(v.Length - 1) do v.[i] <- f i v.[i]
    /// Replaces the elements of vector `v1` by mutating them in place. The new values are computed by applying function `f` to corresponding elements of vectors `v1` and `v2` pairwise. An element index is also supplied to function `f`. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let inline replacei2 (f:int->'T1->'T2->'T1) (v1:Vector<'T1>) (v2:Vector<'T2>):unit = 
        if v1.Length <> v2.Length then invalidArg "" "The vectors should have the same length."
        for i in 0..(v1.Length - 1) do v1.[i] <- f i v1.[i] v2.[i]
    /// Replaces the elements of vector `v1` with the elements of vector `v2`, by mutating them in place. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let inline replaceWith (v1:Vector<'T>) (v2:Vector<'T>):unit = 
        if v1.Length <> v2.Length then invalidArg "" "The vectors should have the same length."
        Array.blit (v2 |> toArray) 0 (v1 |> toArray) 0 v1.Length
    /// Like Vector.fold, but returns the intermediate and final results
    let inline scan (f:'S->'T->'S) (s:'S) (v:Vector<'T>):Vector<'S> = v |> toArray |> Array.scan f s |> ofSeq
    /// Like Vector.foldBack, but returns both the intermediate and final results
    let inline scanBack (f:'T->'S->'S) (s:'S) (v:Vector<'T>):Vector<'S> = Array.scanBack f (v |> toArray) s |> ofSeq
    /// Sets the element of vector`v` with index `i` to value `a`
    let inline set (v:Vector<'T>) (i:int) (a:'T):unit = v.[i] <- a
    /// Returns a sequence of vectors that are obtained by splitting vector `v` into subvectors whose lengths are given in the sequence `n`
    let inline split (n:seq<int>) (v:Vector<'T>):seq<Vector<'T>> = v.Split(n)
    /// Returns a sequence of vectors that are obtained by splitting vector `v` into `n` subvectors of equal length. The length of vector `v` must be an integer multiple of `n`, otherwise ArgumentException is raised.
    let inline splitEqual (n:int) (v:Vector<'T>):seq<Vector<'T>> = v.SplitEqual(n)
    /// Creates a vector with `n` elements, where the `i`-th element is 1 and the rest of the elements are 0
    let inline standardBasis (n:int) (i:int):Vector<'T> = createBasis n i LanguagePrimitives.GenericOne
    /// Creates a new vector that contains the given subrange of vector `v`, specified by start index `s` and count `c`
    let inline sub (v:Vector<'T>) (s:int) (c:int):Vector<'T> = v.GetSubVector(s, c)
*)    /// Returns the sum of all the elements in vector `v`
    let inline sum (v:Vector<'T>):'T = v |> toArray |> Array.sum
(*    /// Returns the sum of the results generated by applying function `f` to each element of vector `v`
    let inline sumBy (f:'T->'U) (v:Vector<'T>):'U = v |> toArray |> Array.sumBy f
    /// Gets the unit vector codirectional with vector `v`
    let inline unitVector (v:Vector<'T>) = v.GetUnitVector()

    *)