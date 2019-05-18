# Test script for f2k
$root = "../.."

function f2k($file) {
    $outfile = "$root/obj/$file.ks"
    $infiles = "$root/$file.fs"
    if (Test-Path $outfile) { Remove-Item -force $outfile }
    $outdir = split-path -parent $outfile
    $null = new-item -Type Directory -force $outdir
    dotnet run .\f2k.fsproj $outfile $infiles
}

f2k "test\f2k\test0"

