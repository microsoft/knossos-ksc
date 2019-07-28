// Learn more about F# at http://fsharp.org

open System
open dnn

#if DiffSharp 

//DiffSharp interface
open F2K_DiffSharp

#else

//Knossos Interface
open Knossos

#endif

[<EntryPoint>]
let main argv =
   
    let ORx = [|0.; 0.;
                0.; 1.;
                1.; 0.;
                1.; 1.;
              |]
              |> Array.map toFloat 
              |> Mat_ofArray 4

    let ORy = [|0.
                1.
                1.
                1.
              |]
              |> Array.map toFloat 
              |> Mat_ofArray 4

    let rng = new System.Random()

    // 2 inputs, one layer with one neuron
    let net = FFLayer.initNet rng [|2; 1|]
    
    let dnn_objective (V:Vec) = 
        let net' = FFLayer.Decode net V
        let ORy' = FFLayer.evalNet net' ORx         
        ORy - ORy'
        |> GetRows
        |> Seq.map sqnorm
        |> Array.ofSeq
        |> Vec_ofArray
        |> Vec_sum
        
    let V0 = FFLayer.Encode net
        
    printfn "Before training: loss = %A" (dnn_objective V0)
            
    #if DiffSharp 

    let V' = F2K_DiffSharp.Minimize 100 (toFloat 0.01) (fun V -> dnn_objective V) V0
    printfn "After training: loss = %A" (dnn_objective V')

    #endif

    0 // return an integer exit code
