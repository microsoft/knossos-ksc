# Use a Single machine to run one command
Param(
  # This needs to be distinct between clients and from other (Azure Batch Graphs) builds
  [Parameter(Mandatory=$True)][string]$BUILD,
  [Parameter(Mandatory=$False)][string[]]$outputs, # defaults to, and DevOps explicitly passes, @("")
  [Parameter(Position=1, ValueFromRemainingArguments=$True, Mandatory=$True)][string[]]$cmd
)

function local:log {
  write-host "azure_batch_single.ps1: $args"
}

Write-host "============================== azure_batch_single.ps1 =============================="

Import-Module "$PSScriptRoot\azure_batch_common.ps1"

$POOL = "knossos-gpu-docker" # These have no startup task, but accept per-task containers.

CreatePool $POOL

log Creating Job and Tasks

$BUILD = CreateJob $BUILD @{
  "poolInfo" = @{ "poolId" = $POOL }
  "onTaskFailure" = "performexitoptionsjobaction"
}

$outputUploaders = $outputs |? {$_ -ne ""} |% {OutputUploader "rlo/outputs/**/$_" $RESULTS_SAS_URL}

$outputUploaders | % { log "OutputUploader: " $_ }

CreateDockerSrcTask $cmd (@{
  "id"= "python"
  "outputFiles" = @(StdErrUploader "") + $outputUploaders
})
az batch job set --job-id $BUILD --on-all-tasks-complete "terminatejob"

WaitForJobCompletion

[void](mkdir results)
# This will set ANY_FAILED to True if the download-batch is unable to download stdout/stderr.
$ANY_FAILED = (!$(az storage blob download-batch --destination "./results" --source "results" --no-progress --sas-token $RESULTS_SAS --pattern "$($BUILD)/*" | Write-Host; $?))
if (CheckTasksDisplayTime) { $ANY_FAILED = $True }

Write-Host STDOUT
type .\results\$BUILD\stdout.txt
Write-Host
Write-Host STDERR
type .\results\$BUILD\stderr.txt
Write-Host

if ($ANY_FAILED) {
    Write-Host
    Write-Host "There were errors; see STDERR above"
    exit 1
}

log All tasks succeeded
Write-host "============================== end azure_batch_single.ps1 =============================="
