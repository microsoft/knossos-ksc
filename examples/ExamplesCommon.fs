// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
module ExamplesCommon

open NUnit.Framework
open Swensen.Unquote
let linspace lo hi n = Array.init n (fun i -> lo + i * ((float hi - lo)/(float n)))

let Array_eq a b = Array.fold2 (fun acc a b -> acc && (a = b)) true a b


[<Test>]
let ``test linspace`` () = 
   test @< Array_eq (linspace 1.0 2.0 3) [|1.0; 1.5; 2.0|] >@
