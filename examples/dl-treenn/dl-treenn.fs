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

/////////////////////////////////////////////////////////////////////////////////////////////////

type WordId = int   // Vocabulary is encoded as one of 60K integers

type WordTree =
    | Leaf of WordId
    | Node of WordTree * WordTree

type TrainingExample = {
    phrase: WordTree
    sentiment: int
}

type SentimentAnalyzerWeights = {
    embedding : Vec []  // 300x60K  Each word in the alphabet has an embedding ...
    Wl : Mat; Wr : Mat  // 300x300  ... and left and right child embeddings combine to give parent embedding
    dense : Mat         // 5x300    ... and the embeddings are mapped to sentiments with a final fully connected layer
}

let sentiment (weights : SentimentAnalyzerWeights) (tree : WordTree) =
    // Local function to walk the tree, accumulating embedding vecs
    let rec embedding tree =
        match tree with
        | Leaf word -> weights.embedding.[word]
        | Node (l,r) -> 
            let sl = embedding l
            let sr = embedding r
            tanh (weights.Wl * sl + weights.Wr * sr)

    // Compute the "embedding" (a 300-dim vector) for the given tree
    let tree_embedding = embedding tree

    // Map tree embedding to sentiment
    softmax (weights.dense * tree_embedding)


// Loss for one example, given hand-labelled sentiment (integer from 1..5)
let loss weights tree sent = crossentropy (sentiment weights tree) sent

// Loss for batch of examples
let loss weights examples = sum [| for ex in examples do yield loss weights ex.phrase ex.sentiment |]

let V = 60000 // Vocabulary size
let H = 300  // Hidden unit size
let weights_init = { 
        embedding = [| for i = 1 to V do yield rand H |]
        Wl = rand (H,H)
        Wr = rand (H,H)
        dense = rand (5, H)
    }

let weights_optimized = adam (fun w -> loss w examples) weights_init

// Do inference
let num_correct validation = sum [| for v in validation do yield if argmax (sentiment weights_optimized v.phrase) = v.sentiment then 1.0 else 0.0 |]
