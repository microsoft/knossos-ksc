// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
///// Mocking
type Vec = float
type Mat = Vec
type Tensor3 = Mat

let sum = Array.sum
let softmax (v : Vec) = 1.1
let argmax (v : Vec) = 1
let crossentropy a b = 1.1
let inline rand p : Vec = 
  1.1 // Array.init p (fun i -> 1.1)

type Real = float

/////////////////////////////////////////////////////////////////////////////////////////////////

let adam f p0 : ('Params -> Real) -> 'Params -> 'Params
    = p0

type AdamParams = { 
    maxiter : int;
    alpha0 : double; 
    beta0 : double; 
    decay_alpha : bool; 
};

// and the signature would be 
let adam aps f p0 : AdamParams -> ('Params -> Real) -> 'Params -> 'Params

// which would be made super-nice by allowing optional arguments, and the construction of a defaulted AdamParams from a partial record, e.g. 
parms = adam { maxiter = 200; decay_alpha = true } f parms
