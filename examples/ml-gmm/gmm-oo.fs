// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
module gmm

#if INTERACTIVE
#load "../../src/f2k/Util.fs";;
#load "../../src/f2k/Vector.fs";;
#load "../../src/f2k/Knossos.fs";;
#endif

open Knossos
open System

// Stable computation of log(sum(exp v)))
let logsumexp (a:Vec) =
    let mx = max a
    let semx = sum (expv (a - mx))
    (log semx) + mx

// Multivariate log gamma
let log_gamma_distrib a p =
    log (Math.PI ** (0.25*(float (p*(p-1))))) + 
        sum (build p (fun j -> gammaLn (a + 0.5*(1. - (float j)))))

// nth triangular number
// map tri [0 1 2 3] = [0 1 3 6]
let tri n = n * (n + 1) / 2


// Type of the Wishart prior parameters
type Wishart = {
    gamma:Double
    m:Int32
}

// Type of a Gaussian,  parametrized by
// mean and covariance, 
// so that likelihood is
//  det(2 pi * Sigma)^-1/2 * exp(-1/2 * (x - mu)' * inv(Sigma) * (x - mu)) 
type Gaussian = {
    mean : Vec;
    cov : Mat;
}

// Type of the square root of a positive definite matrix,
// Lower triangular, with positive entries on the diagonal,
// parametrized by their logs
type SqrtPosDefMat = {
        log_diags : Vec   // Logs of diagonal entries (known positive)
        off_diags : Vec   // Off-diagonal entries, count d*(d-1)/2
    }
    with 
    // Frobenius norm of the Sqrt 
    member this.froNorm () =  sqnorm (expv this.log_diags) + sqnorm this.off_diags

    // Log of determinant of the Sqrt
    member this.logDet () = sum this.log_diags

// Create dense lower triangular matrix from log-diagonal and lower triangle
// Row i=0 : [exp q0,      0,     0,       0,      0]
// Row i=1 : [    l0, exp q1,     0,       0,      0]
// Row i=2 : [    l1,     l2, exp q2,      0,      0]
// Row i=3 : [    l3,     l4,     l5, exp q3,      0]
// Row i=4 : [    l6,     l7,     l8,     l9, exp q4]
type SqrtPosDefMat with member this.asMat () = 
    let d = size this.log_diags
    build2 d d (fun i j ->
        if i < j then
            0.0
        else if i = j then
            exp this.log_diags.[i]
        else
            this.off_diags.[tri (i-1) + j]
    )

(*let test_sqrtpd = 
    let d = 3 in 
    {
        log_diags = rand d
        off_diags = rand (tri d)
    }
    in test_sqrtpd.asMat()*)

// Type of a Gaussian, parametrized by
// mean and inverse-square-root of covariance, 
// so that likelihood is factor (2pi)^(-dim/2) times
//  det(Q) * exp(-1/2 * sumsq(Q * (x - mu)))
type InvSqrtGaussian = {
    mean : Vec
    invSqrtCov : SqrtPosDefMat  // Inverse square root of covariance  
} with
    member this.dim() = size this.mean

// Log likelihood of datum x under InvSqrtGaussian g
// TODO: Add scaling (2pi^(-d/2)) back
type InvSqrtGaussian with member this.log_likelihood (x : Vec) =
    let Q = this.invSqrtCov.asMat ()
    let del = x - this.mean
    let mahal = sqnorm (mul Q del)
    this.invSqrtCov.logDet() - 0.5 * mahal

// Log of Wishart prior of an InvSqrtGaussian
let log_wishart_prior (wishart:Wishart) (g:InvSqrtGaussian) =
    let p = g.dim()
    let n = p + wishart.m + 1
    let C = (float (n*p))*((log wishart.gamma) - 0.5*(log 2.)) - (log_gamma_distrib (0.5*(float n)) p)

    //0.5*(sqr wishart.gamma)*g.invSqrtCov.froNorm() - (float wishart.m)*g.invSqrtCov.logDet() - C
    0.5*(sqrt wishart.gamma)*g.invSqrtCov.froNorm() - (float wishart.m)*g.invSqrtCov.logDet() - C

// ---------- Gaussian Mixture Model (GMM) ----------
type GMM = 
    {
      log_alphas:Vec;              // Logs of mixture weights
      components:InvSqrtGaussian[] // Gaussians
    }
    member this.dim() = size this.components.[0].mean
    member this.num_components() = size this.log_alphas

// Negative log likelihood of GMM
let gmm_objective (gmm:GMM) (x:Vec) =
    let K = gmm.num_components()
    logsumexp( build K (fun k -> 
        gmm.log_alphas.[k] + gmm.components.[k].log_likelihood x)) - 
    (logsumexp gmm.log_alphas) +
        0.5 * sum (build K (fun k -> gmm.components.[k].invSqrtCov.froNorm()))

// Log of Wishart prior
let gmm_log_wishart_prior (wishart:Wishart) (gmm:GMM) =
    Array.sum <| Array.map (fun g -> log_wishart_prior wishart g) gmm.components

// GMM with prior
let gmm_with_prior x gmm wishart =
    gmm_objective gmm x + gmm_log_wishart_prior wishart gmm


// ------ TESTS

let test_gmm = 
    let d = 13 in
    let k = 3 in
    let rand_Gaussian () = 
        {
            mean = rand d
            invSqrtCov = {
                log_diags = rand d
                off_diags = rand (tri d)
            }
        }
    {
        log_alphas = rand k 
        components = Array.init k (fun _ -> rand_Gaussian ())
    }

let test_makeQ = test_gmm.components.[0].invSqrtCov.asMat()



(* -- When we start probabilistic programming
// Sample from GMM
let gmm_sample (rng: RNG) (alphas:Vec) (means:Vec[]) (log_diags:Vec[]) (ls:Vec[]) =
    let K = size alphas
    let k = categorical_sample rng alphas
    let InvSqrtSigma = makeQ log_diags.[k] ls.[k]
    invSqrtGaussian_sample rng Q means.[k]   
*)

(*
// Create upper triangular matrix from log-diagonal and upper triangle
// Nasty and mutating, useful as an instance of something we *should* allow 
// Row i=0 : [exp q0,     l0,     l1,     l2,     l3]
// Row i=1 : [     0, exp q1,     l4,     l5,     l6]
// Row i=2 : [     0,      0, exp q2,     l7,     l8]
// Row i=3 : [     0,      0,      0, exp q3,     l9]
// Row i=4 : [     0,      0,      0,      0, exp q4]
let makeQt (log_diags : Vec) (l : Vec) =
    let d = size q
    let mutable l_index = 0
    build2 d d (fun i j ->
        if i > j then
            0.0
        else if i = j then
            exp log_diags.[i]
        else
            l.[l_index++] // Assume existence of l_index++
    ) 
*)
