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

/// Various utility functions for internal use
module Util

/// Checks whether the 2d array `m` has the same number of elements in both dimensions
let (|Square|) (m:_[,]) =
    match m with
    | m when m.GetLength 0 = m.GetLength 1 -> m
    | _ -> invalidArg "m" "Expecting a square 2d array"

/// Gets the transpose of the 2d array `m`
let inline transpose (m:_[,]) = Array2D.init (m.GetLength 1) (m.GetLength 0) (fun i j -> m.[j, i])

/// Gets an array containing the diagonal elements of the square 2d array `m`
let inline diagonal (Square m:_[,]) = Array.init (m.GetLength 0) (fun i -> m.[i, i])

/// Gets the trace of the square matrix given in the 2d array `m`
let inline trace (m:_[,]) = Array.sum (diagonal m)

/// Copies the upper triangular elements of the square matrix given in the 2d array `m` to the lower triangular part
let inline copyUpperToLower (Square m:_[,]) =
    let r = Array2D.copy m
    let rows = r.GetLength 0
    if rows > 1 then
        for i = 1 to rows - 1 do
            for j = 0 to i - 1 do
                r.[i, j] <- r.[j, i]
    r

/// Finds an array that, when multiplied by an LU matrix `lu`, gives array `b`
let inline matrixSolveHelper (lu:'a[,]) (b:'a[]) =
    let n = lu.GetLength 0
    let x = Array.copy b
    for i = 1 to n - 1 do
        let mutable sum = x.[i]
        for j = 0 to i - 1 do
            sum <- sum - lu.[i, j] * x.[j]
        x.[i] <- sum
    x.[n - 1] <- x.[n - 1] / lu.[n - 1, n - 1]
    for i in (n - 2) .. -1 .. 0 do
        let mutable sum = x.[i]
        for j = i + 1 to n - 1 do
            sum <- sum - lu.[i, j] * x.[j]
        x.[i] <- sum / lu.[i, i]
    x

/// Checks whether a float contains an integer value
let isInteger a = a = float (int a)

// Square a number
let inline sqr x = x * x

