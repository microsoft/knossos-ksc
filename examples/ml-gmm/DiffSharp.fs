module F2K_DiffSharp

open DiffSharp.AD.Float64

type Vec = DV
let toD (x:float) = D x

let sum = DV.sum
let expv = DV.Exp
let max = DV.max
let build p f = DV.init p f 
let gammaLn a = a //Why?
let size (x:DV) = x.Length
let mul (A:DM) (V:DV) = A*V
let sqnorm = DV.L2NormSq
let build2 m n f = DM.init m n f
let exp = D.Exp
let rng = new System.Random()
let rand m = build m (fun _ -> rng.NextDouble())
let rand2 m n = build2 m n (fun i j -> rng.NextDouble())

let Minimize steps (eta:D) (loss:DV->D) (V0:DV) =                    
    [0..steps]
    |> Seq.fold (fun V _ ->
        let _, g = grad' loss V    
        V - eta*g) V0  
 
        