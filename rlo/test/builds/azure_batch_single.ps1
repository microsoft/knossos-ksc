# Use a Single machine to run one command
Param(
  # This needs to be distinct between clients and from other (Azure Batch Graphs) builds
  [Parameter(Mandatory=$True)][string]$BUILD,
  [Parameter(Mandatory=$False)][string[]]$outputs, # defaults to, and DevOps explicitly passes, @("")
  [Parameter(Position=1, ValueFromRemainingArguments=$True, Mandatory=$True)][string[]]$cmd
)

Import-Module "$PSScriptRoot\azure_batch_common.ps1"

function script:log {
  write-host "azure_batch_single.ps1: $args"
}

$POOL = "knossos-gpu-docker" # These have no startup task, but accept per-task containers.

CreatePool $POOL

log "CMD: $cmd"
log Creating Job and Tasks

$BUILD = CreateJob $BUILD @{
  "poolInfo" = @{ "poolId" = $POOL }
  "onTaskFailure" = "performexitoptionsjobaction"
}

CreateDockerSrcTask $cmd (@{
  "id"= "python"
  "outputFiles" = @(StdErrUploader "") + ($outputs |? {$_ -ne ""} |% {OutputUploader "rlo/outputs/**/$_" $RESULTS_SAS_URL})
})
az batch job set --job-id $BUILD --on-all-tasks-complete "terminatejob"

WaitForJobCompletion

[void](mkdir results)
# This will set ANY_FAILED to True if the download-batch is unable to download stdout/stderr.
log "Download results"
az storage blob download-batch --destination "./results" --source "results" --no-progress --pattern "$($BUILD)/*" |`
  Write-Host
$ANY_FAILED = $?
log "Download results: done, exitcode='$?'"
if (CheckTasksDisplayTime) { $ANY_FAILED = $True }

log STDOUT
Get-Content .\results\$BUILD\stdout.txt
log
log STDERR
Get-Content .\results\$BUILD\stderr.txt
log

if ($ANY_FAILED) {
    log
    log "There were errors; see STDERR above"
    exit 1
}

log All tasks succeeded
