module F2K_DiffSharp

open DiffSharp.AD.Float64

type Vec = DV
type Mat = DM
let Float (x:float) = D x

let sum = DV.sum
let expv = DV.Exp
let maximum = DV.max
let build p f = DV.init p f 
let gammaLn a = a //Why?
let size (x:DV) = x.Length
let mvmul (A:DM) (V:DV) = A*V
let sqnorm = DV.L2NormSq
let build2 m n f = DM.init m n f
let exp = D.Exp
let rng = new System.Random()
let rand m = build m (fun _ -> rng.NextDouble())
let rand2 m n = build2 m n (fun _ _ -> rng.NextDouble())

let Minimize steps (eta:D) (loss:DV->D) (V0:DV) =                    
    [0..steps]
    |> Seq.fold (fun V _ ->
        let _, g = grad' loss V            
        V - eta*g) V0  
 
        
let Rows (A:Mat) = A.Rows

let Cols (A:Mat) = A.Cols

let MatToArray (M:Mat) = DM.toArray M    

let VecToArray (V:Vec) = DV.toArray V

let VecOfArray (V:D[]) = toDV V

let MatOfArray n (x:D[]) : Mat = DM.ofArray n x
    
let MatToVec (A:Mat) = DM.ReshapeToDV A

let VecToMat n (V:Vec) :Mat = DM.ofDV n V

let VecConcat (x:seq<Vec>) =  DV.concat x

let CreateRows n (x:Vec) : Mat = DM.createRows n x

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

let VecGetSlice (i:int option, j:int option) (V:Vec) = V.GetSlice (i, j)
   
let MatGetSlice (row_i:int option, row_j:int option, col_i:int option, col_j:int option) (A:Mat) = A.GetSlice (row_i, row_j, col_i, col_j)

let ElemProd (A:Mat) (A':Mat) = A .* A'

let Minus (x:float) (A:Mat) = x - A