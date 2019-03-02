{---------------------------------------------------}
{- Can we get to linear time with simple rewrites? -}

-- 1. inline once.  
-- In Knosssos proper, there will be an `if` in the function body, 
-- so we won't do the magic x1:x2:xs pattern change
grad_prod_slow_1 [_] = [1.0]
grad_prod_slow_1 (x1:[x2]) = 
    (prod [x2] : map (* x1) [1.0]) 
grad_prod_slow_1 (x1:x2:xs) = 
    (prod (x2:xs) : map (* x1) (prod xs : map (* x2) (grad_prod_slow_1 xs))) 

-- 2. Apply rule `prod (x:xs) = x * prod xs`.  
grad_prod_slow_2 [_] = [1.0]
grad_prod_slow_2 (x1:[x2]) = 
    (x2 : [x1]) -- Some simplifications here, not important to flow
grad_prod_slow_2 (x1:x2:xs) = 
    (x2 * prod xs : map (* x1) (prod xs : map (* x2) (grad_prod_slow_2 xs))) 

-- 2.a CSE -- no good, still quadratic
grad_prod_slow_3 [_] = [1.0]
grad_prod_slow_3 [x1, x2] = [x2, x1]
grad_prod_slow_3 (x1:x2:xs) = 
    let p = prod xs in 
    (x2 * p : map (* x1) (p : map (* x2) (grad_prod_slow_3 xs))) 

-- 3. "Passdown"
grad_prod_slow_4 [_] = [1.0]
grad_prod_slow_4 [x1, x2] = [x2, x1]
grad_prod_slow_4 prod_xs (x1:x2:xs) = 
    (x2 * prod_xs : map (* x1) (prod_xs : map (* x2) (grad_prod_slow_4 xs))) 
