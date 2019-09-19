// TODO: rename to oo-fcdnn.fs
module dnn

#if DiffSharp 

//DiffSharp interface
open F2K_DiffSharp

#else

//Knossos Interface
open Knossos

#endif

let Dropout (rnd:System.Random) p (A:Mat) =     
    A |> Mat_map (fun x -> if rnd.NextDouble() < p then toFloat 0.0 else x) 
    
type FFLayer =
    { W:Mat          // Weight matrix
      b:Vec          // Bias vector
      a:Mat->Mat     // Activation function
      dropout : float 
    }                     
    static member evalDrop rnd (x:Mat) (l:FFLayer) = 
        let activation = l.a >> Dropout rnd l.dropout
        let B = Mat_createRows x.Rows l.b
        x * l.W + B |> activation 

    member this.EvalDrop rnd (x:Mat) = FFLayer.evalDrop rnd x this                

    static member eval (x:Mat) (l:FFLayer) =         
        let B = Mat_createRows x.Rows l.b    
        x * l.W + B |> l.a

    member this.Eval (x:Mat) = FFLayer.eval x this                

    static member initDrop a n_in n_out d (rnd:System.Random) = 
        { W = Glorot rnd n_in n_out
          b = Vec_zero n_out 
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

    static member Encode (layers:FFLayer[]) = layers |> Seq.collect(fun l -> [Vec_ofMat l.W; l.b]) |> Vec_concat
    
    static member EncodedLength (layer:FFLayer) = layer.W.Rows*layer.W.Cols + layer.b.Length

    static member NetworkEncodedLength (layers:FFLayer[]) = 
        layers
        |> Array.fold(fun acc l -> acc + (FFLayer.EncodedLength l)) 0
        
    static member Decode (layers0:FFLayer[]) (V:Vec) = 
        let lengths = layers0 |> Array.map FFLayer.EncodedLength
        let from = Array.init (layers0.Length+1) (fun i -> if i = 0 then 0 else lengths.[0..i-1] |> Array.sum)
        layers0 
        |> Array.mapi(fun i l -> 
            let n = l.W.Rows * l.W.Cols
            let W = V.[from.[i]..from.[i]+n-1] |> Mat_ofVec l.W.Rows
            let b = V.[from.[i]+n..from.[i+1]-1]
            {layers0.[i] with W = W; b= b})
 
