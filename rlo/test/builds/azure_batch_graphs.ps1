# Distributed graphs build, each rep on its own machine, configurable to run multiple experiments
Param(
    [Parameter(Mandatory=$False)][Int]$num_repetitions=10,
    [Parameter(Mandatory=$False)][Int]$DAYS_TO_LIVE=1,
    [Parameter(Position=1, ValueFromRemainingArguments=$True, Mandatory=$True)][string[]]$scenarios_flags
)
# Build name needs to be distinct from the other (Azure Batch Profiling) build. Use build- rather than e.g. graphs-
# for historical consistency.
$BUILD="build-$($env:BUILD_BUILDNUMBER)"
Import-Module "$PSScriptRoot\azure_batch_common.ps1"

$POOL = "knossos-gpu-docker" # These have no startup task, but accept per-task containers.

CreatePool $POOL

Write-Host Creating Job and Tasks

$build_args="--gitlog $(git rev-parse --short HEAD) --num_repetitions $($num_repetitions)"
[System.Collections.ArrayList]$scenarios = @()
$scenarios_flags |% {
  if ($_.StartsWith("-")) {
    $build_args += " " + $_
  } else {
    [void]$scenarios.Add($_)
  }
}

$BUILD = CreateJob $BUILD (@{
  "poolInfo" = @{ "poolId" = $POOL }
  "onTaskFailure" = "performexitoptionsjobaction" # Can't be specified by command-line, hence json. Hopefully will give us decent defaults for task ExitOptions.
  "usesTaskDependencies" = $True # --uses-task-dependencies
  })

$scenarios |% {
  $scenario = $_
  $run_id = "$($scenario)_$($env:BUILD_BUILDNUMBER)"
  [System.Collections.ArrayList]$ids = @()
  for ($rep=0; $rep -lt $num_repetitions; $rep++) {
    $id = "$($scenario)_$($rep)"
    CreateDockerSrcTask "python src/train_over_expressions.py $($scenario) --run_id $($run_id) $($build_args) --repetition $rep --force_gpu" (@{
      "id"= $id
      "outputFiles" =@(
        (StdErrUploader "Run_$($run_id)/$($rep)"),
        (OutputUploader "outputs/**/events*.json.bz2" $RESULTS_SAS_URL)
        (OutputUploader "outputs/**/*" $VERBOSE_SAS_URL)
      )
    })
    [void]$ids.Add($id) #Add returns the length of the list, so discard
  }
  # The plotting task consumes the output of the other jobs in the experiment via Azure Blob Storage;
  # the dependsOn means they must have finished+uploaded their output before we download.
  # We rename the build-<buildnum> directory from blob storage to 'outputs' as a local run would expect to find; this contains Run_...
  $downloadLogs = (DownloadCmd "$($BUILD)/Run_$($run_id)/**/events*.json.bz2")
  $plot_cmd = "python src/summarize_logs.py $($scenario) --run_id $($run_id) $($build_args)"
  CreateDockerSrcTask "$($downloadLogs); mv $($BUILD) outputs; $($plot_cmd)" (@{
    "id" = "$($scenario)_plot"
    "dependsOn" = @{
      "taskIds" = $ids
    }
    "outputFiles" = @(
      (StdErrUploader "Run_$($run_id)"),
      (OutputUploader "outputs/**/config.json" $RESULTS_SAS_URL), # This is the only run that actually saves the config
      (OutputUploader "outputs/**/train.pck" $RESULTS_SAS_URL),
      (OutputUploader "outputs/**/*.png" $RESULTS_SAS_URL),
      (OutputUploader "outputs/**/*.html" $RESULTS_SAS_URL),
      (OutputUploader "outputs/**/*.kso" $RESULTS_SAS_URL),
      (OutputUploader "outputs/**/*.mp4" $RESULTS_SAS_URL)
    )
  })
}
az batch job set --job-id $BUILD --on-all-tasks-complete "terminatejob"

WaitForJobCompletion

# Download results onto local machine in order to publish (in a later build step)
[void](mkdir results)

Write-Host Downloading into results/...
@("config.json","train.pck", "*.html","*.png", "*.kso", "*.mp4") |% {
  if (!$(az storage blob download-batch --destination "./results" --source "results" --no-progress --pattern "$($BUILD)/**/$_" | Write-Host; $?)) {
    $ANY_FAILED = $True
  }
}

#Annoyingly that puts everything in results/$BUILD
mv results/$BUILD/* results/
[void](rmdir results/$BUILD)

if (CheckTasksDisplayTime) { $ANY_FAILED = $True }

$RESULTS_URL=az storage blob url --container-name "results" --name $BUILD
Write-Host Leaving logs at $RESULTS_URL SAS+URL $RESULTS_SAS_URL prefix $BUILD
Write-Host "Download logs with (maybe add --dryrun):"
Write-Host "PS> az storage blob download-batch --source results --destination . --pattern '$($BUILD)/**/*.json' --sas-token '$RESULTS_SAS' --account-name $($env:AZURE_STORAGE_ACCOUNT)"
Write-Host "bash> az storage blob download-batch --source results --destination . --pattern '$($BUILD)/**/*.json' --sas-token $RESULTS_SAS --account-name $($env:AZURE_STORAGE_ACCOUNT)"

if ($ANY_FAILED) {
  [void](mkdir errors)
  Write-Host Downloading stdout/stderr...
  az storage blob download-batch --destination "./errors" --source "results" --no-progress --pattern "$($BUILD)/**/std*.txt"
  mv errors/$BUILD/* errors/
  [void](rmdir errors/$BUILD)
  exit 1
}

Write-Host All tasks succeeded
