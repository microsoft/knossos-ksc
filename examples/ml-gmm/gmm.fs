module gmm

open DV

type Vec = Vector<float>
type Mat = Vector<Vector<float>>

let inline size (v: Vector<'T>) = v.Length

let inline build n (f : int -> 'T) = Vector.init n f
let inline build2 m n (f : int -> int -> float) = Vector.init m (fun i -> Vector.init n (f i))
let inline sum v = Vector.sum v
let inline mul (a: Vector<Vector<float>>) (b: Vector<float>) : Vector<float> = 
    sum(build (size a) (fun i -> a.[i] * b.[i]))

let inline max (a: Vector<float>) = a.GetMaxBy( fun x->x )
let inline exp (a: Vector<float>) = Vector.map exp a
let inline sqnorm (a: Vector<'a>) = a.GetL2NormSq()

let logsumexp (a:Vec) =
    let mx = max a
    let semx = sum (exp (a - mx))
    (log semx) + mx

let log_gamma_distrib a p =
    log (Math.Pow(Math.PI,(0.25*(float (p*(p-1)))))) + 
        sum (build p (fun j -> SpecialFunctions.GammaLn (a + 0.5*(1. - (float j)))))

// ---------- Gaussian Mixture Model (GMM) ----------

// Nth triangular number
let tri n = n * (n - 1) /2

// Create lower triangular matrix from log-diagonal and lower triangle
let makeQ (q : Vec) (l : Vec) =
    let d = size q
    build2 d d (fun i j ->
        if i < j then
            0.0
        else if i = j then
            exp q.[i]
        else
            l.[tri (i - 1) + j]
    )

// Negative log likelihood of GMM
let gmm_objective (x:Vec[]) (alphas:Vec) (means:Vec[]) (qs:Vec[]) (ls:Vec[]) =
    let n = x.Length
    let K = size alphas
    sum ( build n (fun i ->
          logsumexp( build K (fun k -> 
            let Q = makeQ qs.[k] ls.[k]
            let dik = x.[i] - means.[k]
            let mahal = mul Q dik |> sqnorm
            alphas.[k] + sum (qs.[k]) - 0.5 * mahal)
    ))) - 
    (float n) * (logsumexp alphas) +
        0.5 * sum (build K (fun k -> sqnorm (exp qs.[k]) + sqnorm ls.[k]))

// Log of Wishart prior
let log_wishart_prior p wishart_gamma wishart_m (qs:Vec[]) (ls:Vec[]) =
    let K = qs.Length
    let n = p + wishart_m + 1
    let C = (float (n*p))*((log wishart_gamma) - 0.5*(log 2.)) - (log_gamma_distrib (0.5*(float n)) p)

    let out = sum (build K (fun ik -> 
                        let frobenius1 = sqnorm qs.[ik] // TODO: exp?
                        let frobenius2 = sqnorm ls.[ik]
                        0.5*wishart_gamma*wishart_gamma*(frobenius1+frobenius2) - (float wishart_m)*(sum qs.[ik]) // TODO: sum...
    ))
    out - (float K)*C

// GMM with prior
let gmm_with_prior x alphas means qs ls wishart_gamma wishart_m =
    gmm_objective x alphas means qs ls + 
    log_wishart_prior (size x.[0]) wishart_gamma wishart_m qs ls

// Sample from GMM
let gmm_sample (rng: RNG) (alphas:Vec) (means:Vec[]) (qs:Vec[]) (ls:Vec[]) =
    let K = size alphas
    let k = categorical_sample rng alphas
    let InvSqrtSigma = makeQ qs.[k] ls.[k]
    invSqrtGaussian_sample rng Q means.[k]   

