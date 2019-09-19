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
$dir = split-path $PSCommandPath
$proj = "$dir/src/f2k/f2k.fsproj"
dotnet run --project $proj $proj $out $args
