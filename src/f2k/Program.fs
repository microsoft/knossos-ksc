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
    let fsproj = "nul.fsproj" // unused?

    (* see https://fsharp.github.io/FSharp.Compiler.Service/project.html *)
    let sysLib nm = 
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
            // file references only valid on Windows
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFiles) +
            @"\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.2.0\ref\netcoreapp2.2\" + nm + ".dll"
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
                               
                                   //This is effective a stub library. We should also check the files exist
                                   yield "Util.fs"
                                   yield "Vector.fs"
                                   yield "Knossos.fs"

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

    checkProjectResults.AssemblyContents.ImplementationFiles

[<EntryPoint>]
let main argv =
    if argv.Length < 3 then
        printfn "usage: f2k outfile infiles"
        exit 1
    
    let outFile = argv.[1]
    let files = argv.[2..]

    if File.Exists(outFile) then
        failwithf "Error: Will not overwrite existing file %s\n" outFile

    printfn "f2k: Parsing %d files to %s" (Seq.length files) outFile

    (* e.g. run as:
       dotnet run .\f2k.fsproj ..\..\..\examples\ml-gmm\gmm.fs  
    *)

    for f in files do
        if not (File.Exists(f)) then
            failwithf "Cannot open file %A" f

    let checkedFiles = parseAndCheckFiles files
    // checkedFiles now also contains the stub libraries, so take only the last N 
     
    let prelude = seq {
        yield ";; Prelude: Knossos.ks"
        yield! File.ReadAllLines "Knossos.ks"
    }
    let decls =
        checkedFiles
        |> Seq.skip (Seq.length checkedFiles - Seq.length files)
        |> Seq.collect (fun implementationFileContent -> seq {            
            yield ";; SRC: " + implementationFileContent.FileName
            yield! lispgen.toLispDecls implementationFileContent
        })
    printfn "f2k: Writing %d + %d lines to file %s" (Seq.length prelude) (Seq.length decls) outFile
    File.WriteAllLines (outFile, (Seq.append prelude decls))
    0
