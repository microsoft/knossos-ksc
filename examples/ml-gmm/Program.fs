// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
// Learn more about F# at http://fsharp.org

open System
open gmm
open Knossos

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
    let ls = Array.init K (fun _ -> rand (gmm.tri (D - 1)))
    printfn "Hello %A" (gmm.gmm_objective x alphas means qs ls)
    0 
