module test0


//open knossos

let e (x : int) = 1.3

let f (x : float) = x + 4.0

let g x y = (f x) * y + System.Math.Sin x

let h x = if x > 0.2 then g x 3.0 else f x

