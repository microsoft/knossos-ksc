// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
// Learn more about F# at http://fsharp.org

open System
open gmm

#if DiffSharp 

//DiffSharp interface
open F2K_DiffSharp

#else

//Knossos Interface
open Knossos

#endif

//  gmm_objective (x:Vec[]) (alphas:Vec) (means:Vec[]) (qs:Vec[]) (ls:Vec[]) =

[<EntryPoint>]
let main argv =
    let N = 100
    let K = 10
    let D = 3
    let x = Array.init N (fun _ -> rand D)
    let alphas = rand K 
    let means = Array.init K <| fun _ -> rand D
    let qs = Array.init K (fun _ -> rand D)
    //let ls = Array.init K (fun _ -> rand (gmm.tri (D - 1)))
    let ls = Array.init K (fun _ -> rand (gmm.tri D))
    printfn "Initial %A" (gmm.gmm_objective x alphas means qs ls)
    
#if DiffSharp 

    //let alphas' = F2K_DiffSharp.Minimize 100 (Float 0.01) (fun a -> gmm.gmm_objective x a means qs ls) alphas
    //printfn "Optimized %A" (gmm.gmm_objective x alphas' means qs ls)

#endif

    0 
