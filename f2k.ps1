# Script to run f2k on fs files
param([switch]$del=$false, [string]$out)

if ($del -and (test-path $out)) {
    write-host "Deleting $out"
    del $out
}
cd src\f2k
dotnet run f2k "..\..\$out" ($args | % { "../../$_" })
