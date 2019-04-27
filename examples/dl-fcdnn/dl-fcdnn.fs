// "Artist's rendition" of simple FC DNN in DF#. 
open FP      // Functional programming primitives: polymorphic zip, fold, etc
open Tensor  // Vector/Matrix/Tensor, as plain-ol-data types

type Mat = Matrix<double>
type Vec = Vector<double>

// Pretend that Tensor didn't define relu
let relu x = max x 0.0

// Define a fully-connected (FC) DNN.
// The distinction between matrix and tensor doesn't matter for 
// FC models so images can just be reshaped to 1D vectors
let dnn (weights : Mat list) (x : Vec) =
    let mutable ret = x
    for W in weights do
        ret <- relu (W * ret)
    ret

// Alternative, more F#y/LINQy rendition.  
// Both are good, and it's important that we can do both
let dnn2 (weights : Mat list) (x : Vec) =
    foldr (fun W x -> relu (W * x)) weights x

// Autoencoder loss for one example
let autoencoder_loss weights x = sqnorm (x - dnn weights x)
 
// Autoencoder loss for a batch of examples
let autoencoder_loss weights examples = 
   sum [for input in examples do (loss weights input)]
 
// Initialize weights
let weights_init = [Mat.rand 784 400, Mat.rand 400 200, Mat.rand 200 784]
 
// In the best world, we don’t need this
// let grad_loss weights examples = 
//  [for input in examples do 
//      (Diff (fun w -> (loss w input)) weights)] 
//  |> sum
    
// We just do this:
weights_trained = adam (w -> loss w examples) weights_init
 





// Even more F#y rendition.  
// We may never want to show this outside of the functional programming community.
let (<||) f g x y = f (g x y) // > // Define "blackbird" combinator
let dnn3 (weights : Mat list) (x : Vec) =
    foldr (relu <|| (*)) weights x
