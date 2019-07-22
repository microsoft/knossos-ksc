

$ver = "0.0.1"

# Package VS code extension and install in "$extensions_dst"
$extensions_dst = "~\.vscode\extensions\knossos-vscode-$ver"

$manifest = echo `
    CHANGELOG.md `
    language-configuration.json `
    package.json `
    README.md `
    syntaxes\Knossos.tmLanguage

write-host "ks-vscode: Deleting $extensions_dst"
Remove-Item -force -rec $extensions_dst
foreach ($file in $manifest) {
    $dst = "$extensions_dst\$file"
    mkdir -force (Split-Path $dst) >$null
    Copy-Item $file $dst
    write-host "ks-vscode: Copied $dst"
}
