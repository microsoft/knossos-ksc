module F2K_DiffSharp

open DiffSharp.AD.Float64

type Float = D
type Vec = DV
type Mat = DM
type RNG = System.Random

let toFloat (x:float) = D x

// These are now in a more .NET-like form, where we might write Vec.sum
// For now, underscore replaces the dot.
let Vec_sum = DV.sum
let Vec_exp = DV.Exp
let Vec_maximum = DV.max
let Vec_init p f = DV.init p f 
let Mat_init m n f = DM.init m n f

let Vec_rand (rng : RNG) m = Vec_init m (fun _ -> rng.NextDouble())
let Mat_rand (rng : RNG) m n = Mat_init m n (fun _ _ -> rng.NextDouble())

let Vec_zero m = Vec_init m (fun _ -> toFloat 0.0)
let Mat_zero m n = Mat_init m n (fun _ _ -> toFloat 0.0)

let Vec_one m = Vec_init m (fun _ -> toFloat 1.0)
let Mat_one m n = Mat_init m n (fun _ _ -> toFloat 1.0)

let Mat_toArray (M:Mat) = DM.toArray M    

let Vec_toArray (V:Vec) = DV.toArray V

let Vec_ofArray (V:D[]) = toDV V

let Mat_ofArray n (x:D[]) : Mat = DM.ofArray n x
    
let Mat_map (f : Float -> Float) (A : Mat) =
    A 
    |> Mat_toArray 
    |> Array.map f 
    |> Mat_ofArray A.Rows

let Minimize steps (eta:D) (loss:DV->D) (V0:DV) =                    
    [0..steps]
    |> Seq.fold (fun V _ ->
        let _, g = grad' loss V            
        V - eta*g) V0  

let Vec_ofMat (A:Mat) = DM.ReshapeToDV A

let Mat_ofVec n (V:Vec) :Mat = DM.ofDV n V

let Vec_concat (x:seq<Vec>) =  DV.concat x

let Mat_createRows n (x:Vec) : Mat = DM.createRows n x


let exp = D.Exp

let sqnorm = DV.L2NormSq

let gammaLn a = a // TODO: implement gammaLn correctly

let reLU (A:Mat) = DiffSharp.Util.reLU A

let tanh (A:Mat) = tanh A

let sigmoid (A:Mat) = DiffSharp.Util.sigmoid A

let GetRows (A:Mat) = DM.toRows A

let GetCols (A:Mat) = A.GetCols()    

let MatOfRows (V:Vec[]) = DM.ofRows V

let Transpose (A:Mat) = DM.Transpose A

let MatOfCols (V:Vec[]) = DM.ofCols V

let MatMul (A:Mat) (A':Mat) = A*A'

let MatAppendCols (A:Mat) (A':Mat) = Seq.append (A.GetCols()) (A'.GetCols())  |> DM.ofCols

let Vec_getSlice (i:int option, j:int option) (V:Vec) = V.GetSlice (i, j)
   
let Mat_getSlice (row_i:int option, row_j:int option, col_i:int option, col_j:int option) (A:Mat) = A.GetSlice (row_i, row_j, col_i, col_j)
       
let ElemProd (A:Mat) (A':Mat) = A .* A'

let Glorot (rnd:System.Random) x y = 
    let l = sqrt(6.0 / (float (x + y)))
    Mat_init x y (fun _ _ -> l*(2.0*rnd.NextDouble() - 1.0) |> toFloat)
