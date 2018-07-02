let vggblk2 w n_channels n_convs x =
  conv2 w n_channels 3 1 1 |>
  conv2 w n_channels 3 1 1 |> (
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
