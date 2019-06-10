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
let inline fbool b = if b then 1.0 else 0.0
let adam f w = w
let foldr = List.foldBack
let size (v : Vec) = 1
let head (x :: xs) = x
let inline rand sz = 1.0

// mapAccumR (name is from Haskell) folds over a list, and returns a list of partial results
let inline mapAccumR (f : 't -> 'acc -> 'acc * 's) xs s0 = 
   foldr (fun (x : 't) ((s, ys) : 'acc * 's list) -> let (sprime, y) = f x s in (sprime, y :: ys)) xs (s0, [])

let dup x = (x,x)

/////////////////////////////////////////////////////////////////////////////////////////////////

type Weights = {
    Wx : Mat  // Transform input to hidden
    Wh : Mat  // Transform hidden to hidden
    b  : Vec  // Bias hidden to hidden
}

// RNN model: Given input sequence xs, compute output sequence
let rnn (weights : Weights) (xs : Vec list)  =                                     
    let mutable h = rand (size weights.b)
    [
      for x in xs do
        h <- tanh (weights.Wh * h) + tanh (weights.Wx * x) + weights.b 
        yield h 
    ]

let weights = { Wx = rand (5,10); Wh = rand (5,5); b = rand 5 }                     

// RNN model: Given input sequence xs, compute output sequence ys
let rnn3 (weights : Weights) (xs : Vec list) =   
    foldr (fun x (h, ys) -> 
            let h = tanh (weights.Wh * h) + tanh (weights.Wx * x) + weights.b
            (h, h :: ys))
          xs 
          (rand (size weights.b), []) |> snd

// RNN model: Given input sequence xs, compute output sequence
let rnn2 (weights : Weights) (xs : Vec list)  =                                     
    let h0 = rand (size weights.b) in
    snd <| mapAccumR (fun h x -> dup <| tanh (weights.Wh * h) + tanh (weights.Wx * x) + weights.b) xs h0 
