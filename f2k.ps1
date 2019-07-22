# Script to run f2k on fs files
param([switch]$del=$false, [string]$out)

# Delete the output file if existing and -del
if (test-path $out) {
	if ($del) {
		write-host "Deleting $out"
		Remove-Item $out
	} else {
		throw "Output file $out exists, not deleting.  Call with '-del' to force delete."
	}
}
Set-Location src\f2k
dotnet run f2k "..\..\$out" ($args | % { "../../$_" })
Set-Location ../..
