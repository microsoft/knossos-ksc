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
    //RNN demo: count the number of G's in a DNA sequence (to avoid encoding/decoding bases are represented as ints)
    let DnaSeqLength = 30
    let TrainingSamples = 100
    let TestingSamples = 20
    let TotalSamples = TrainingSamples + TestingSamples
    let rng = new System.Random(42)

    let RndDna n = Array.init n (fun _ -> rng.Next(4))

    let data = Array.init TotalSamples (fun _ -> RndDna DnaSeqLength)
    let labels = data |> Array.map(fun s -> s |> Seq.filter(fun c -> c=0) |> Seq.length)

    let Split n (X:'a[]) = X.[0..n-1], X.[n..]
    let training_data, testing_data = Split TrainingSamples data
    let training_labels, testing_labels = Split TrainingSamples labels

    let encodeData (D:int[][]) = 
        Array.init D.[0].Length (fun i -> D |> Array.map (fun x -> x.[i] |> float |> toFloat) |> Mat_ofArray D.Length)
    let encodeLabels (L:int[]) = L |> Array.map (float >> toFloat) |> Mat_ofArray L.Length

    let net = GRU.init rng 1 1 // size of hidden = 1; size of input = 1
    
    let rnn_objective (X:Mat[]) (Y:Mat) (V:Vec) = 
        let net' = GRU.Decode net V
        let Y' = GRU.eval X net'
        Y - Y'
        |> GetRows
        |> Seq.sumBy sqnorm

    let V0 = GRU.Encode net
    let X = encodeData training_data
    let X' = encodeData testing_data
    let Y = encodeLabels training_labels
    let Y' = encodeLabels testing_labels

    let print_predictions (V:Vec) = 
        let net' = GRU.Decode net V
        let pred = GRU.eval X' net' |> Vec_ofMat
        let ToDna (S:seq<int>) = 
            S 
            |> Seq.map (fun s -> 
                match s with 
                | 0 -> 'G' 
                | 1 -> 'C' 
                | 2 -> 'A' 
                | 3 -> 'T' 
                | _ -> failwith "Unknown base") 
            |> Array.ofSeq 
            |> String

        testing_data
        |> Array.iteri (fun i x -> printfn "%s:%i" (ToDna x) (pred.[i] |> float |> int))
                    
    printfn "Before training" 
    printfn "\tTraining loss: %A" (rnn_objective X Y V0)
    printfn "\tValidation loss: %A" (rnn_objective X' Y' V0)
    print_predictions V0
            
    #if DiffSharp 
    let V' = F2K_DiffSharp.Minimize 100 (toFloat 0.01) (fun V -> rnn_objective X Y V) V0
    printfn "Before training" 
    printfn "\tTraining loss: %A" (rnn_objective X Y V')
    printfn "\tValidation loss: %A" (rnn_objective X' Y' V')
    print_predictions V'
    #endif        

    0 // return an integer exit code
