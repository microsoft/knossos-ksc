module dnn

#if DiffSharp 

//DiffSharp interface
open F2K_DiffSharp

#else

//Knossos Interface
open Knossos

#endif

let Dropout (rnd:System.Random) p (A:Mat) =     
    A
    |> MatToArray 
    |> Array.map (fun x -> if rnd.NextDouble() < p then Float 0.0 else x) 
    |> MatOfArray (Rows A)
       
let Glorot (rnd:System.Random) x y = 
    let l = sqrt(6.0 / (float (x + y)))
    build2 x y (fun _ _ -> l*(2.0*rnd.NextDouble() - 1.0) |> Float)

type FFLayer =
    { W:Mat          // Weight matrix
      b:Vec          // Bias vector
      a:Mat->Mat     // Activation function
      dropout : float 
    }                     
    static member evalDrop rnd (x:Mat) (l:FFLayer) = 
        let activation = l.a >> Dropout rnd l.dropout
        let B = (CreateRows (Rows x) l.b)
        (MatMul x l.W) + B |> activation 

    member this.EvalDrop rnd (x:Mat) = FFLayer.evalDrop rnd x this                

    static member eval (x:Mat) (l:FFLayer) =         
        let B = (CreateRows (Rows x) l.b)    
        //NOTE: we want (x*l.W + B |> l.a) but this causes a problem with Vector instead of Matrix multiply due to the types
        (MatMul x l.W) + B |> l.a

    member this.Eval (x:Mat) = FFLayer.eval x this                

    static member initDrop a n_in n_out d (rnd:System.Random) = 
        { W = Glorot rnd n_in n_out
          b = build n_out (fun _ -> Float 0.0) 
          a = a
          dropout = d
        }            
      
    static member init a n_in n_out (rnd:System.Random) = FFLayer.initDrop a n_in n_out 0.0 rnd        

    //Network methods
    static member initNetDrop rnd d (layerSizes:int[]) =      
        Array.init (layerSizes.Length - 1) 
            (fun i -> 
                let activation = if i = layerSizes.Length-2 then id else reLU                    
                FFLayer.initDrop activation layerSizes.[i] layerSizes.[i+1] d rnd
            )            

    static member initNet rnd (layerSizes:int[]) =  FFLayer.initNetDrop rnd 0.0 layerSizes

    static member evalNet (layers:FFLayer[]) (x:Mat) = Array.fold FFLayer.eval x layers    

    static member evalNetDrop rnd (layers:FFLayer[]) (x:Mat) = Array.fold (FFLayer.evalDrop rnd) x layers    

    static member Encode (layers:FFLayer[]) = layers |> Seq.collect(fun l -> [MatToVec l.W; l.b]) |> VecConcat
    
    static member VectorLength (layer:FFLayer) = (Rows layer.W)*(Cols layer.W) + layer.b.Length

    static member NetworkVectorLength (layers:FFLayer[]) = 
        layers
        |> Array.fold(fun acc l -> acc + (FFLayer.VectorLength l)) 0
        
        
    static member Decode (layers0:FFLayer[]) (V:Vec) = 
        let lengths = layers0 |> Array.map FFLayer.VectorLength
        let from = Array.init (layers0.Length+1) (fun i -> if i = 0 then 0 else lengths.[0..i-1] |> Array.sum)
        layers0 
        |> Array.mapi(fun i l -> 
            let n = (Rows l.W)*(Cols l.W)
            let W = V.[from.[i]..from.[i]+n-1] |> VecToMat (Rows l.W)
            let b = V.[from.[i]+n..from.[i+1]-1]
            {layers0.[i] with W = W; b= b})
 
