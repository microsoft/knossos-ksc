// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
type Mat = float
let rand () = System.Random().NextDouble()
let Mat_rand n m = 3.2

// A simple linear-layer tree model (LLTM)
type Tree = Leaf of double | Node of Tree * Tree 

// I like a record, could more compactly use a pair
type LLTMParams = {
    L: Mat; 
    R: Mat 
}

// Combining my children's latent representation to form my own
let combine parms l r = parms.L * l + parms.R * r

// Define the lltm model:
let rec lltm parms x = 
    match x with 
    | Leaf x -> x
    | Node (l,r) -> combine parms (lltm parms l) (lltm parms r)

// Initialize parameters
let parms = { L = Mat_rand 10 10; R = Mat_rand 10 10 }

// Function to make a random tree
let rec tree() = if rand() < 0.5 then Leaf (rand()) else Node (tree(), tree())
tree()

// Run the model on a random tree
lltm parms (tree())






// Pure function to make a random tree
let rec ptree rng = if rand rng < 0.5 then Leaf (rand rng) else Node (ptree (next rng), ptree (next rng))

