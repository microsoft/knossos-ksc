$ver = "0.0.1"

# Package VS code extension and install in "$extensions_dst"
$extensions_dst = "~\.vscode\extensions\knossos-vscode-$ver"

$srcdir = split-path $PSCommandPath

$manifest = echo `
    CHANGELOG.md `
    language-configuration.json `
    package.json `
    README.md `
    syntaxes\Knossos.tmLanguage `
    out\knossos_ir_formatter.js `
    out\extension.js

write-host "ks-vscode: Deleting $extensions_dst"
Remove-Item -force -rec $extensions_dst
foreach ($file in $manifest) {
    $dst = "$extensions_dst\$file"
    mkdir -force (Split-Path $dst) >$null
    $src = join-path $srcdir $file
    Copy-Item $src $dst
    write-host "ks-vscode: Copied $dst from $src"
}
