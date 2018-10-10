module MF_CurveFitting

// B-Spline basis function of degree p, segment i
let rec b_spline_basis_segment (i:int) (p:int) (t:float) = 
    let ti = floor t
    if p = 0 then
        if i = int ti then 1.0 else 0.0
    else
        let dt = t - float i
        (dt*(b_spline_basis_segment i (p-1) t) + 
         (float (p+1) - dt)*(b_spline_basis_segment (i+1) (p-1) t))/(float p)

// B-Spline basis function of degree p, with 
let b_spline_basis (p:int) (t:float) = 
    b_spline_basis_segment 0 p (t + float (p+1) / 2.)

