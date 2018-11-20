
let inline rand p = 
  Array.init 2 (fun i -> 1.1)

type Vec = float[]
type Mat = Vec[]
type Tensor3 = Mat[]



let conv2 (w:Mat) (x:Vec) = x
let mmul (a:Mat) (b:Vec) = b
let relu (x:Vec) = x
let dropout (x:Vec) = x
let softmax (x:Vec) = x

type vgg_weights = { 
  Wconv: Mat seq   // Weight matrices for convolutional layers
  Wfull: Mat[2]    // Weight matrices for fully connected layers
}

// A vgg16 network (applied wlog to 1D images).  
// Apply weights to image x
let vgg16 weights x =
    let vggblk Ws x = foldr (conv2 >> relu) W x |> max_pooling_2d
    foldr vggblk weights.Wc x |>
    mmul weights.Wfc.[0] |> relu |> dropout |>
    mmul weights.Wfc.[1] |> relu |> dropout |>
    softmax

// Network "architecture" is defined by the
// function 'vgg16' and shapes of the weight matrices
let vggblk_init n_channels n_convs = seq {
    yield rand (128*128, n_channels, 3, 1, 1)
    repeat (n_convs - 1) (fun _ -> 
      yield rand (n_channels, n_channels, 3, 1, 1)
    )
  }

let vgg16_init x_size = {
    Wc = seq {
      vggblk_init  64 2
      vggblk_init 128 2
      vggblk_init 256 3 
      vggblk_init 512 3 
      vggblk_init 512 3 
      vggblk_init 512 3 
    }; 
    Wfc = [|
      rand (None, 4096)
      rand (4096, 4096)
    |]
  }



let xvggblk_init n_channels n_convs =
    seq { 
      yield rand (None, n_channels, 3, 1, 1)
      yield rand (n_channels, n_channels, 3, 1, 1) 
      if n_convs = 3 then
        yield rand (n_channels, n_channels, 3, 1, 1)
    }

