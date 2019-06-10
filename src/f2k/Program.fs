// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
// Learn more about F# at http://fsharp.org
#if INTERACTIVE
#r @"C:\Users\Andrew Fitzgibbon\.nuget\packages\fsharp.compiler.service\25.0.1\lib\netstandard2.0\FSharp.Compiler.Service.dll"
#endif

open FSharp.Compiler.SourceCodeServices
open lispgen
open System.IO

// Create an interactive checker instance 
let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parseAndCheckFiles files = 
    let fsproj = @"c:\dev\github\knossos\test\f2k\test0.fsproj" // unused?

    (* see https://fsharp.github.io/FSharp.Compiler.Service/project.html *)
    let sysLib nm = 
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
            // file references only valid on Windows
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFiles) +
            @"\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.1.0\ref\netcoreapp2.1\" + nm + ".dll"
        else
            let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
            let (++) a b = System.IO.Path.Combine(a,b)
            sysDir ++ nm + ".dll" 

    let directory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

    let localLib name =
        System.IO.Path.Combine(directory, name) + ".dll"

    // Get context representing a stand-alone (script) file
    let projOptions = checker.GetProjectOptionsFromCommandLineArgs
                            (fsproj,
                                [| yield "--simpleresolution" 
                                   yield "--noframework" 
                                   yield "--debug:full" 
                                   yield "--define:DEBUG" 
                                   yield "--optimize-" 
                                   yield "--doc:test.xml" 
                                   yield "--warn:3" 
                                   yield "--fullpaths" 
                                   yield "--flaterrors" 
                                   yield "--target:library"
                                   yield! files
                                   let references =
                                     [ sysLib "mscorlib" 
                                       sysLib "System"
                                       sysLib "System.Core"
                                       sysLib "System.Runtime"
                                       sysLib "System.Runtime.Extensions"
                                       localLib "FSharp.Core" ]
                                   for r in references do 
                                         yield "-r:" + r
                                   |])
    
    let checkProjectResults = checker.ParseAndCheckProject(projOptions) |> Async.RunSynchronously
    if checkProjectResults.Errors <> [||] then
        let s = checkProjectResults.Errors |> Array.iter (fun (e:FSharpErrorInfo) -> printf "%A: %A\n" e.Severity (e.ToString()))

        if checkProjectResults.Errors|> Array.exists (fun error -> error.Severity = FSharpErrorSeverity.Error) then
            failwith "There were errors"

    checkProjectResults.AssemblyContents.ImplementationFiles |> List.last

[<EntryPoint>]
let main argv =
    let outFile = argv.[1]
    let prefixedFiles = argv.[2..]

    printfn "f2k: Parsing %d files to %s" (Seq.length prefixedFiles) outFile

    (* e.g. run as:
       dotnet run .\f2k.fsproj (echo Util Vector Knossos gmm | % { "..\..\examples\ml-gmm\$_.fs" })  
    *)

    for f in prefixedFiles do
        if not (File.Exists(f)) then
            failwithf "Cannot open file %A" f

    let checkedFile = parseAndCheckFiles prefixedFiles
    let decls = lispgen.toLispDecls checkedFile
    printfn "f2k: Writing %d lines to file %s" (Seq.length decls) outFile
    File.WriteAllLines (outFile, decls)
    0
