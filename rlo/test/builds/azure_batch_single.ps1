# Use a Single machine to run one command
Param(
  # This needs to be distinct between clients and from other (Azure Batch Graphs) builds
  [Parameter(Mandatory=$True)][string]$BUILD,
  [Parameter(Mandatory=$False)][string[]]$outputs, # defaults to, and DevOps explicitly passes, @("")
  [Parameter(Position=1, ValueFromRemainingArguments=$True, Mandatory=$True)][string[]]$cmd
)

function script:log {
  write-host "azure_batch_single.ps1: $args"
}

log "BUILD: $BUILD"
log "CMD: $cmd"
log "outputs: [$outputs]"

Import-Module "$PSScriptRoot\azure_batch_common.ps1"

$POOL = "knossos-gpu-docker" # These have no startup task, but accept per-task containers.

CreatePool $POOL

log "Creating Job and Tasks"

$BUILD = CreateJob $BUILD @{
  "poolInfo" = @{ "poolId" = $POOL }
  "onTaskFailure" = "performexitoptionsjobaction"
}

$outputsinsubdir = $outputs |? {$_ -ne ""} |% {"rlo/outputs/**/$_"}

# https://stackoverflow.com/questions/54346256/fileuploadmiscerror-azure-batch-output-file
$outputFiles = @(StdErrUploader "") + `
  @(FilePatternUploader "" "../fileupload*.txt") + `
  @($outputsinsubdir |% {OutputUploader $_ $RESULTS_SAS_URL})

log "outputFiles: begin"
write-host $outputFiles
log "outputFiles: end"

$task = CreateDockerSrcTask $cmd (@{
  "id"= "python"
  "outputFiles" = $outputFiles
})
log "task=" ($task | convertfrom-json)

az batch job set --job-id $BUILD --on-all-tasks-complete "terminatejob"

WaitForJobCompletion

[void](mkdir results)
# This will set ANY_FAILED to True if the download-batch is unable to download stdout/stderr.
log "Download results"
az storage blob download-batch --destination "./results" --source "results" --pattern "$($BUILD)/*" |`
  Write-Host
if (-not $?) {
  log "Failed to download results"
  exit 1
}
log "Download results: done"

function emitlog($name) {
  log "Begin dump of $name"
  Get-Content .\results\$BUILD\$name.txt | ForEach-Object { log "${name}: $_"}
  log "End dump of $name"
}
emitlog STDOUT
emitlog STDERR
emitlog FILEUPLOADOUT
emitlog FILEUPLOADERR

$ANY_FAILED = CheckTasksDisplayTime

if ($ANY_FAILED) {
    log
    log "There were errors; see log above"
    exit 1
}

log "All tasks succeeded"
