// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
open Knossos

let a (x : float) = 3.0 * x

let b (y : float) = 2.0 * x

let g (x : float) (y : float) = x * y

// f = 3z/2y for z=5x so f = 15x/2y so linear in x
let f1 (x : float) (y : float) = let z = 5.0 * x in (a z) / (b y)

let f (x : float) (y : float) = x / y

let main =
    let x = 1.1
    let y = 2.2
    let delta = 0.0001

    ( 
        f (a 3.0) (b 2.3),
        f 0.0 1.0,
        g x y,
        "FD=",
        ((f x (y + delta)) - (f x y)) / delta,
        //(fwd$f x y delta delta)
        //"CHECK=" ($check f1 rev$f1 (tuple x y) (tuple delta delta) 0.1)
        "DONE"
    )

