#load "Paket.fsx"



let f (x:float) (y:float) : float = 
    let a = 2. * x + 7. * y
    let b = a/y
    let c = 3.*a*b + 5.*x
    c
    
// "Handwritten" grad:
let grad_f1 (x:float) (y:float) : float * float = 
    let a = 2. * x + 7. * y
    let a_x,a_y = 2.,7.
    let b = a/y
    let b_x,b_y = a_x/y, -a*a_y/y**2.
    (* let c = 3.*a*b + 5.*x *)  // We don't need to compute c for grad
    let c_x,c_y = 3.*a_x*b + 3.*a*b_x + 5., 3.*a_y*b + 3.*a*b_y
    c_x,c_y


