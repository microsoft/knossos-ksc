

let conv2_do w p q r s x = 
    // do convolution ..
    w * x

let conv2_init w p q r s x = 
    // Append random weights to incoming source of weights w
    // Or read from deserialization structure or whatever
    (rand (size x) p q r s) :: w

let vggblk w n_channels n_convs x =
  conv2 w n_channels 3 1 1 |> relu |>
  conv2 w n_channels 3 1 1 |> relu |> (
  if n_convs = 3 then
    conv2 w n_channels 3 1 1
  else
    identity
  ) |> 
  relu |>
  max_pooling_2d 2 2

let vgg16 w x =
  x |>
  vggblk w 64 2 |>
  vggblk w 128 2 |>
  vggblk w 256 3 |>
  vggblk w 512 3 |>
  vggblk w 512 3 |>
  vggblk w 512 3 |>
  mmul w None 4096 |> relu |> dropout |>
  mmul Wfc.[2] |> relu |> dropout |>
  softmax

let w_init = vgg16 init ()

let cost w = 
    map2 ( fun xi yi -> crossEntropy (vgg16 w xi) yi) ) training_examples

let w_trained = sgd (cost w)
