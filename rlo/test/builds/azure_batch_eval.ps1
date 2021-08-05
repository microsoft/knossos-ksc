Param(
    [Parameter(Mandatory=$False)][int]$num_repetitions=10, # Must be the same as the original build
    [Parameter(Position=1, ValueFromRemainingArguments=$True, Mandatory=$True)][string]$experiment
)

# This needs to be distinct from the other (Azure Batch Graphs) build so they don't conflict,
# but TODO we would also like some way for the plotting build to use results from either
# (and the replay build *must* have a different buildnum from the replayed models...)
$BUILD="eval-$($env:BUILD_BUILDNUMBER)"
Import-Module "$PSScriptRoot\azure_batch_common.ps1"

Write-Host Creating Job and Tasks

$BUILD = CreateJob $BUILD @{
  "poolInfo" = @{ "poolId" = $POOL }
  "onTaskFailure" = "performexitoptionsjobaction"
}

$orig_build = $experiment -replace '.*_','build-'

for ($rep=0; $rep -lt $num_repetitions; $rep++) {
  # We need to download quite a few files...
  $cmdLine = [String]::Join("; ", @(
    (DownloadCmd "$($orig_build)/Run_$($experiment)/config.json"),
    (DownloadCmd "$($orig_build)/Run_$($experiment)/train.pck"),
    (DownloadCmd "$($orig_build)/Run_$($experiment)/$($rep)/**/*.npz" "verbose" $VERBOSE_SAS),
    "mkdir outputs",
    "mv $($orig_build)/* outputs/",
    "python src/evaluate_saved_models.py $($experiment) --repetition $($rep)"))
  CreateDockerSrcTask $cmdLine (@{
    "id"= $rep
    "outputFiles" =@(
      (StdErrUploader $rep),
      (OutputUploader "outputs/**/*.json" $RESULTS_SAS_URL) # This'll give us (newbuildnum)/(old_runid)/.../eval_events.json
    )
  })
}
az batch job set --job-id $BUILD --on-all-tasks-complete "terminatejob"

WaitForJobCompletion

if (CheckTasksDisplayTime) {
  [void](mkdir errors)
  Write-Host "Tasks failed; downloading stdout/stderr..."
  az storage blob download-batch --destination "./errors" --source "results" --no-progress --pattern "$($BUILD)/**/std*.txt"
  mv errors/$BUILD/* errors/
  [void](rmdir errors/$BUILD)
  Write-Host "There were errors, published as artifact"
  exit 1
}

Write-Host All tasks succeeded