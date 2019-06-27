// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
// Learn more about F# at http://fsharp.org
#if INTERACTIVE
#r @"C:\Users\Andrew Fitzgibbon\.nuget\packages\fsharp.compiler.service\25.0.1\lib\netstandard2.0\FSharp.Compiler.Service.dll"
#endif

//We're avoiding requiring projects files so we specify the exact location, is there a better way of doing this?
let extendedPath = @"dotnet/sdk/NuGetFallbackFolder/microsoft.netcore.app/2.2.0/ref/netcoreapp2.2/"

open FSharp.Compiler.SourceCodeServices
open lispgen
open System.IO

// Create an interactive checker instance 
let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parseAndCheckFiles files = 
    let fsproj = "nul.fsproj" // unused?

    let (++) a b = System.IO.Path.Combine(a,b)

    (* adapted for .NET Core from https://fsharp.github.io/FSharp.Compiler.Service/project.html *)
    let sysLib nm =
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then

            let windowsReferencePath =
                System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFiles) ++ extendedPath
            if not (Directory.Exists windowsReferencePath) then
                failwithf "Can't find .NET Core references, ensure they're installed. Looking in%s%s" System.Environment.NewLine windowsReferencePath 
            windowsReferencePath ++ nm + ".dll"
        else

            let otherPath = "/usr/share/" ++ extendedPath //WSL/Ubuntu install
            let otherPathLocal = "/usr/share/local/" ++ extendedPath //macOS install

            if Directory.Exists otherPath then
                otherPath ++ nm + ".dll"
            elif Directory.Exists otherPathLocal then
                otherPathLocal ++ nm + ".dll"
            else
                failwithf
                    "Can't find .NET Core references, ensure they're installed. Looking in%s%s%s%s"
                    System.Environment.NewLine
                    otherPath
                    System.Environment.NewLine
                    otherPathLocal
            
    let directory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

    let localLib name =
        directory ++ name + ".dll"

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
    let prefixedFiles = argv.[2..]

    if File.Exists(outFile) then
        failwithf "Error: Will not overwrite existing file %s\n" outFile

    printfn "f2k: Parsing %d files to %s" (Seq.length prefixedFiles) outFile

    (* e.g. run as:
       dotnet run .\f2k.fsproj (echo gmm | % { "..\..\..\examples\ml-gmm\$_.fs" })  
    *)

    for f in prefixedFiles do
        if not (File.Exists(f)) then
            failwithf "Cannot open file %A" f

    let checkedFiles = parseAndCheckFiles prefixedFiles
    let decls =
        checkedFiles
        |> Seq.collect (fun implementationFileContent -> seq {
            yield ";" + implementationFileContent.FileName
            yield! lispgen.toLispDecls implementationFileContent
        })
    printfn "f2k: Writing %d lines to file %s" (Seq.length decls) outFile
    File.WriteAllLines (outFile, decls)
    0
