# Script to run f2k on fs files
param([switch]$del=$false, [string]$out)

if (test-path $out) {
	if ($del) {
		write-host "Deleting $out"
		del $out
	} else {
		throw "Output file $out exists, not deleting.  Call with '-del' to force delete."
	}
}
cd src\f2k
dotnet run f2k "..\..\$out" ($args | % { "../../$_" })
cd ../..

