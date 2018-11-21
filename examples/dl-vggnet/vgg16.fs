
type Vec = float[]
type Mat = Vec[]
type Tensor3 = Mat[]

let inline rand p = 
  Mat (Array.init 2 (fun i -> 1.1))

let W = 128
let H = 128

let conv2 (w:Mat) (x:Vec) = x
let mmul (a:Mat) (b:Vec) = b
let relu (x:Vec) = x
let dropout (x:Vec) = x
let softmax (x:Vec) = x
let max_pooling_2d (x:Vec) = x
let foldr (f: 'y -> 'x -> 'x ) (ys: 'y[]) (x : 'x) =  Array.foldBack f ys x
let (<||) f g x y = f (g x y)         // >
let repeat n f = seq { yield (f 1) }

type vgg_weights = { 
  Wconv: Mat[][]   // Weight matrices for convolutional layers
  Wfull: Mat[]     // Weight matrices for fully connected layers
}

// A vgg16 network (applied wlog to 1D images).  
// Apply weights to image x
let vgg16 weights x =
    let vggblk Ws x = foldr (relu <|| conv2) Ws x |> max_pooling_2d
    foldr vggblk weights.Wconv x |>
    mmul weights.Wfull.[0] |> relu |> dropout |>
    mmul weights.Wfull.[1] |> relu |> dropout |>
    softmax

// Network "architecture" is defined by the
// function 'vgg16' and shapes of the weight matrices
let vggblk_init n_channels n_convs =
    seq { 
      yield rand (W*H, n_channels, 3, 1, 1)
      yield rand (n_channels, n_channels, 3, 1, 1) 
      if n_convs = 3 then
        yield rand (n_channels, n_channels, 3, 1, 1)
    } |> Seq.toArray

let vgg16_init x_size = {
    Wconv = [|
      vggblk_init  64 2
      vggblk_init 128 2
      vggblk_init 256 3 
      vggblk_init 512 3 
      vggblk_init 512 3 
      vggblk_init 512 3 
    |] 
    Wfull = [|
      rand (None, 4096)
      rand (4096, 4096)
    |]
  }



