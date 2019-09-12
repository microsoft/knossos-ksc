// Learn more about F# at http://fsharp.org

open System
open rnn


#if DiffSharp 

//DiffSharp interface
open F2K_DiffSharp

#else

//Knossos Interface
open Knossos

#endif

[<EntryPoint>]
let main argv =
    //RNN demo: count the fraction G's in a DNA sequence (to avoid encoding/decoding bases are represented as ints)
    let DnaSeqLength = 30
    let TrainingSamples = 100
    let TestingSamples = 20
    let TotalSamples = TrainingSamples + TestingSamples
    
    let RndDna n = Array.init n (fun _ -> rng.Next(4) |> float)

    let data = Array.init TotalSamples (fun _ -> RndDna DnaSeqLength)
    let labels = data |> Array.map(fun s -> s |> Seq.filter(fun c -> c=0.0) |> Seq.length |> float |> fun x -> x/(float DnaSeqLength))

    let Split n (X:'a[]) = X.[0..n-1], X.[n..]
    let training_data, testing_data = Split TrainingSamples data
    let training_labels, testing_labels = Split TrainingSamples labels

    let encodeData (D:float[][]) = 
        Array.init D.[0].Length (fun i -> D |> Array.map (fun x -> x.[i] |> Float) |> MatOfArray D.Length)
    let encodeLabels (L:float[]) = L |> Array.map Float |> MatOfArray L.Length
    

    let net = GRU.init rng 1 1 // size of hidden = 1; size of input = 1
    
    let rnn_objective (X:Mat[]) (Y:Mat) (V:Vec) = 
        let net' = GRU.Decode net V
        let Y' = GRU.eval X net'
        Y - Y'
        |> GetRows
        |> Seq.map sqnorm
        |> Array.ofSeq
        |> VecOfArray
        |> sum


    let V0 = GRU.Encode net
    let X = encodeData training_data
    let X' = encodeData testing_data
    let Y = encodeLabels training_labels
    let Y' = encodeLabels testing_labels

    let print_predictions (V:Vec) = 
        let net' = GRU.Decode net V
        let pred = GRU.eval X' net' |> MatToVec
        let ToDna (S:seq<float>) = 
            S 
            |> Seq.map (fun s -> 
                match s with 
                | 0.0 -> 'G' 
                | 1.0 -> 'C' 
                | 2.0 -> 'A' 
                | 3.0 -> 'T' 
                | _ -> failwith "Unknown base") 
            |> Array.ofSeq 
            |> String

        testing_data
        |> Array.iteri (fun i x -> printfn "%s\t%f\t%f" (ToDna x) (pred.[i] |> float) testing_labels.[i])                
                    
    printfn "Before training" 
    printfn "\tTraining loss: %A" (rnn_objective X Y V0)
    printfn "\tValidation loss: %A" (rnn_objective X' Y' V0)
    print_predictions V0
            
    #if DiffSharp 
    let V' = F2K_DiffSharp.Minimize 1000 (Float 0.01) (fun V -> rnn_objective X Y V) V0
    printfn "After training" 
    printfn "\tTraining loss: %A" (rnn_objective X Y V')
    printfn "\tValidation loss: %A" (rnn_objective X' Y' V')
    print_predictions V'
    #endif        

    0 // return an integer exit code
