param($basename = $(throw "need basename"), [switch]$noprelude = $False)

$basename = $basename -replace "\.ks$",""
$objname = "obj/$basename"

set-alias ksc .\dist-newstyle\build\x86_64-windows\ghc-8.4.4\knossos-0.0.0.1\x\ksc\build\ksc\ksc.exe

if ($noprelude) {
    $prelude = @()
} else {
    $prelude = @("--ks-source-file", "src/runtime/prelude.ks")
}

ksc --compile-and-run `
    @prelude `
    --ks-source-file "$basename.ks" `
    --ks-output-file "$objname.kso"  --cpp-output-file "$objname.cpp" `
    --c++ g++  `
    --exe-output-file "$objname.exe"
