// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
module gmm

open System
open System.Diagnostics
open System.IO
open SpecialFunctions
open MathNet.Numerics

type Wishart = {gamma:Double; m:Int32}
type vec = float[]

// Stable computation of log(sum(exp v)))
let inline logsumexp (arr:float[]) =
    let mx = Array.max arr
    let semx = Array.map (fun x -> exp(x-mx)) arr |> Array.sum
    (log semx) + mx

// Multivariate log gamma
let vgammaln a p =
    log (Math.Pow(Math.PI,(0.25*(float (p*(p-1)))))) + 
        ([for j = 1 to p do yield SpecialFunctions.GammaLn (a + 0.5*(1. - (float j)))] |> List.sum)

// square
let square x = x*x

// 2-norm squared
let normSquared v = Array.map square v |> Array.sum


// nth triangular number (0 1 3 6)
// tri 0 = 0
// tri 1 = 1
// tri 2 = 3
// tri 3 = 6
let tri n = n * (n+1) / 2

// GMM: Gaussian Mixture Model
// See eqn 2 of https://github.com/awf/autodiff/blob/master/Documents/AD%202016%20extended.pdf
// alphas
//   vector of logs of mixture weights (unnormalized), so
//   weights = exp(alphas)/sum(exp(alphas))
// means  k x d 
//   Matrix of component means
// qs     k x d 
//   Matrix of logs of diagonals of component inverse covariance matrices
// ls     k x d*(d-1)/2
//   Matrix of vectorized lower triangles of component inverse covariance matrices

// Assemble lower-triangular matrix from log-diagonal and lower triangle
// [ exp(q1)        0        0        0 ]
// [      l1  exp(q2)        0        0 ]
// [      l2       l3  exp(q3)        0 ]
// [      l4       l5       l6  exp(q4) ]
let makeQ (q : Vector) (l : Vector) =
    let d = length q
    Matrix.init d d (fun i j ->
      if i < j then
        0.0
      else if i = j then
        exp q.[i]
      else
        l.[tri (i - 1) + j]
    )

// Assemble lower-triangular matrix from log-diagonal and lower triangle and multiply by vector v
// [ exp(q0)        0        0        0 ]
// [      l0  exp(q2)        0        0 ]
// [      l1       l2  exp(q3)        0 ]
// [      l3       l4       l5  exp(q4) ]
let mulQv (q : Vector) (l : Vector) (v : Vector) =
    Vector.init (length v) (fun i ->
      let tis = tri (i - 1)
      let sum = 
        iterateNumber (fun acc idx -> 
          let j = idx - tis
          let isInRange = j >= 0 && j < i
          if (isInRange) then 
            l.[idx] * v.[j]
          else 
            0.0
        ) 0.0 (Card 0) (length l)
      sum + exp(q.[i]) * v.[i]
    )

//let Qtimesv_test () =
  //let q = vec3 0.1 -1.0 0.3
  //let l = vec3 5.0 -2.0 7.1
  //let v = vec3 1.4 -7.0 3.1
  //let ans0 = exp(q.[0]) * v.[0]
  //let ans1 =      l.[0] * v.[0] + exp(q.[1]) * v.[1]
  //let ans2 =      l.[1] * v.[0] +      l.[2] * v.[1] + exp(q.[2]) * v.[2]
  //let ans = vec3 ans0 ans1 ans2
  //let qv = Qtimesv q l v
  //let nrm = sqnorm (vectorSub qv ans)
  ////assert (nrm < 0.0001)
  //numberPrint nrm
let log_wishart_prior p (wishart:Wishart) (qs:vec[]) (icf:vec[]) =
    let n = p + wishart.m + 1
    let k = icf.Length
    let C = (float (n*p))*((log wishart.gamma) - 0.5*(log 2.)) - (vgammaln (0.5*(float n)) p)
    let out = List.sum [ 
        for ik = 0 to k-1 do
            let frobenius1 = normSquared qs.[ik] 
            let frobenius2 = normSquared icf.[ik].[p..]
            yield 0.5*(square wishart.gamma)*(frobenius1+frobenius2) - (float wishart.m)*sum_qs.[ik]
    ]
    out - (float k)*C
        
let gmm_objective (alphas:float[]) (means:float[][]) (icf:float[][]) (x:float[][]) (wishart:Wishart) =
    let k = alphas.Length
    let d = means.[0].Length
    let n = x.Length

    let CONSTANT = 1. / (pown (sqrt (2. * Math.PI)) d)
    let Qdiags = [|for curr_icf in icf do yield (Array.map exp curr_icf.[..(d-1)])|]
    let sum_qs = [|for curr_icf in icf do yield (Array.sum curr_icf.[..(d-1)])|]
    
    let Q_times_x (Qdiag:float[]) (curr_icf:float[]) (curr_x:float[]) =
        let mutable res = Array.map2 (*) Qdiag curr_x
        let mutable curr_icf_idx = d
        for i = 0 to d-1 do
            for j = i+1 to d-1 do
                res.[j] <- res.[j] + curr_icf.[curr_icf_idx] * curr_x.[i]
                curr_icf_idx <- curr_icf_idx + 1
        res

    let main_term (curr_x:float[]) ik =
        let xcentered = Array.map2 (-) curr_x means.[ik]
        let sqsum_mahal = (Q_times_x Qdiags.[ik] icf.[ik] xcentered)
                            |> Array.map (fun x -> x*x)
                            |> Array.sum
        -0.5*sqsum_mahal
    
    let mutable slse = 0. 
    for curr_x in x do
        let tmp = Array.map2 (+) (Array.map2 (+) alphas sum_qs) [| for i = 0 to k-1 do yield (main_term curr_x i)|]
        slse <- slse + (logsumexp tmp)

    slse + (float n) * ((log CONSTANT) - (logsumexp alphas)) + (log_wishart_prior d wishart Qdiags sum_qs icf)
    
/////// Derivative extras ////////
let log_wishart_prior_ p (wishart:Wishart) (Qdiags:float[][]) (sum_qs:float[]) (icf:float[][]) =
    let log_gamma_distrib a p =
        log (Math.Pow(Math.PI,(0.25*(float (p*(p-1)))))) + 
            ([for j = 1 to p do yield SpecialFunctions.GammaLn (a + 0.5*(1. - (float j)))] |> List.sum)
    let square x = x*x
    let n = p + wishart.m + 1
    let k = icf.Length
    let C = (float (n*p))*((log wishart.gamma) - 0.5*(log 2.)) - (log_gamma_distrib (0.5*(float n)) p)

    let main_term ik =
        let frobenius1 = Qdiags.[ik] 
                            |> Array.map square
                            |> Array.sum
        let frobenius2 = icf.[ik].[p..]
                            |> Array.map square
                            |> Array.sum
        0.5*wishart.gamma*wishart.gamma*(frobenius1+frobenius2) - (float wishart.m)*sum_qs.[ik]

    let out = [for ik = 0 to k-1 do yield (main_term ik)] |> List.sum
    out - (float k)*C

let vectorize alphas means icf =
    Array.append (Array.append alphas [|for mean in means do for elem in mean do yield elem|])
            [|for curr_icf in icf do for elem in curr_icf do yield elem|]

let reshape inner_dim outer_dim (arr:float[]) =
    [|for i = 0 to outer_dim-1 do yield arr.[(i*inner_dim)..((i+1)*inner_dim-1)]|]

#if DO_GMM_FULL
let gmm_objective_ (alphas:D[]) (means:D[][]) (icf:D[][]) (x:float[][]) (wishart:Wishart)  =
    let k = alphas.Length
    let d = means.[0].Length
    let n = x.Length

    let CONSTANT = 1. / (pown (sqrt (2. * Math.PI)) d)
    let Qdiags = [|for curr_icf in icf do yield (Array.map exp curr_icf.[..(d-1)])|]
    let sum_qs = [|for curr_icf in icf do yield (Array.sum curr_icf.[..(d-1)])|]
    
    let Q_times_x (Qdiag:D[]) (curr_icf:D[]) (curr_x:D[]) =
        let mutable res = Array.map2 (*) Qdiag curr_x
        let mutable curr_icf_idx = d
        for i = 0 to d-1 do
            for j = i+1 to d-1 do
                res.[j] <- res.[j] + curr_icf.[curr_icf_idx] * curr_x.[i]
                curr_icf_idx <- curr_icf_idx + 1
        res

    let main_term (curr_x:float[]) ik =
        let xcentered = Array.map2 (-) curr_x means.[ik]
        let sqsum_mahal = (Q_times_x Qdiags.[ik] icf.[ik] xcentered)
                            |> Array.map (fun x -> x*x)
                            |> Array.sum
        -0.5*sqsum_mahal
        
    let slse = [for curr_x in x do 
                    yield (logsumexp 
                            (Array.map3 (fun x y z -> x+y+z) 
                                alphas sum_qs [| for i = 0 to k-1 do yield (main_term curr_x i)|]))
                    ] 
                    |> List.sum

    slse + (float n) * ((log CONSTANT) - (logsumexp alphas)) + (log_wishart_prior_ d wishart Qdiags sum_qs icf)

let gmm_objective_wrapper d k (parameters:float[]) (x:float[][]) (wishart:Wishart) =
    let means_off = k
    let icf_sz = d*(d + 1) / 2
    let icf_off = means_off + d*k
    let means = (reshape d k parameters.[means_off..(means_off+d*k-1)])
    let icf = (reshape icf_sz k parameters.[icf_off..(icf_off+icf_sz*k-1)])
    gmm_objective_ parameters.[..(k-1)] means icf x wishart
#endif

#if DO_GMM_SPLIT
/////// Derivative extras extras - fixing stack overflow ////////
let gmm_objective_1 (alphas:D[]) (means:D[][]) (icf:D[][]) (x:float[])  =
    let k = alphas.Length
    let d = means.[0].Length
    let n = x.Length
    
    let Q_times_x (curr_icf:float[]) (curr_x:float[]) =
        let mutable res = [|for i = 0 to d-1 do yield (exp(curr_icf.[i]) * curr_x.[i]) |]
        let mutable curr_icf_idx = d
        for i = 0 to d-1 do
            for j = i+1 to d-1 do
                res.[j] <- res.[j] + curr_icf.[curr_icf_idx] * curr_x.[i]
                curr_icf_idx <- curr_icf_idx + 1
        res

    let main_term (curr_x:float[]) ik =
        let xcentered = Array.map2 (-) curr_x means.[ik]
        let mahal = Q_times_x icf.[ik] xcentered
        let sqsum_mahal = Array.sum (Array.map (fun x -> x*x) mahal)
        let sumlog_Ldiag = Array.sum icf.[ik].[..(d-1)]
        alphas.[ik]  + sumlog_Ldiag - 0.5*sqsum_mahal

    logsumexp [| for i = 0 to k-1 do yield (main_term x i)|]
    
let gmm_objective_2 d n (alphas:D[]) (icf:D[][]) (wishart:Wishart)  =
    let CONSTANT = 1. / (pown (sqrt (2. * Math.PI)) d)
    let Qdiags = [|for curr_icf in icf do yield (Array.map exp curr_icf.[..(d-1)])|]
    let sum_qs = [|for curr_icf in icf do yield (Array.sum curr_icf.[..(d-1)])|]
    (float n) * ((log CONSTANT) - (logsumexp alphas)) + (log_wishart_prior_ d wishart Qdiags sum_qs icf)
    
let gmm_objective_1wrapper d k (parameters:float[]) (x:float[]) =
    let means_off = k
    let icf_sz = d*(d + 1) / 2
    let icf_off = means_off + d*k
    gmm_objective_1 parameters.[..(k-1)] (reshape d k parameters.[means_off..(means_off+d*k-1)])
                    (reshape icf_sz k parameters.[icf_off..(icf_off+icf_sz*k-1)]) x

let gmm_objective_2wrapper d k n (parameters:float[]) (wishart:Wishart) =
    let means_off = k
    let icf_sz = d*(d + 1) / 2
    let icf_off = means_off + d*k
    gmm_objective_2 d n parameters.[..(k-1)] (reshape icf_sz k parameters.[icf_off..(icf_off+icf_sz*k-1)]) wishart
#endif