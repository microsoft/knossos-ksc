// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
open FP      // Functional programming primitives: polymorphic zip, fold, etc
open Tensor  // Vector/Matrix/Tensor, 
open MLFuncs // relu, crossentropy etc
open Bytes   // asInt32Array etc

type Mat = Matrix<double>
type Vec = Vector<double>

// Define a fully-connected (FC) DNN.
// The distinction between matrix and tensor doesn't matter for 
// FC models so images can just be reshaped to 1D vectors
let dnn (weights : Mat list) (x : Vec) =
    foldr (fun W x -> relu (W * x)) weights x

// Label loss for one example
let label_loss weights x label =
    crossentropy (dnn weights x) label
 
// Label loss for a batch of images and labels
let label_loss weights images labels =
    seq { for (image,label) in zip images labels do 
            label_loss weights image label 
    } |> sum
 
// Download and extract images
let mnist_train_images = 
    wget "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz" |> 
    unzip |> mnist_image_reader
 
// And labels
let mnist_train_labels = 
    wget "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz" |> 
    unzip |> mnist_label_reader
 
// Initialize weights
let mnist_weights_init = [Mat.rand 10 400, Mat.rand 400 200, Mat.rand 200 784]
 
// And train
mnist_weights_trained = adam (w -> label_loss w mnist_images mnist_labels) weights_init


// Look, lovely mnist reader code, even though it's a custom format
let mnist_image_reader (bytes : string) =
    assert (asInt32BE bytes 1 0 == 0x801)
    let num_images = asInt32BE bytes 1 4
    let w, h = asInt32BE bytes 1 8, asInt32BE bytes 1 12
    seq {
        for i in 0..num_images-1 do 
            yield Vec.create (w*h) (asUInt8 bytes (w*h) (12 + i * w * h))
    }
 
let mnist_label_reader (bytes : uint8[]) =
    assert (asInt32BEBE bytes 0 == 0x803)
    let num_labels = asInt32BE bytes 4
    asArrayUInt8 bytes num_labels 4
