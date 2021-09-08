function script:log {
  write-host "azure_batch_common.ps1: $args"
}

# Common functions, variables and setup for azure_batch builds.
# Note many of these use global variables in the module; they have not been made fully reusable or parameterized.
az batch account login --name knossosbuildpipeline --resource-group adobuilds --subscription Knossos

# If DAYS_TO_LIVE isn't set, set it.
if ($DAYS_TO_LIVE -eq $null) {
  log "Using default DAYS_TO_LIVE 1 for SAS URLs"
  $DAYS_TO_LIVE = 1
}

function AzBatchCreate {
  Param(
    [string]$what, # Typically job or task
    $props,
    [Parameter(mandatory=$False, ValueFromRemainingArguments=$True)]$rest
  )
  $TmpFile = New-TemporaryFile
  $props | ConvertTo-Json -Depth 9 | Out-File $TmpFile.FullName
  # This writes to stdout, but returns the exitcode
  $success = $(az batch $what create --json-file $TmpFile.FullName @rest | Write-Host; $?)
  Remove-Item -force $TmpFile.FullName
  # If exitcode indicates failure, the error message is already on stdout
  if (!$success) {
    throw "Failed to create $what"
  }
}

function CreateJob {
  # Create a job, given job_id and properties. Throws if it can't.
  # Unless force_name is specified, adds a suffix to job_id chosen so that there is no job with that ID
  # Return the id of the created job
  Param(
    [string]$job_id,
    [switch]$force_name,
    $props
  )
  if ((!$force_name) -and (az batch job show --job-id $job_id)) {
    $base = $job_id
    do {
      log "Job $job_id exists, trying new id"
      $suffix += 1 # Will set to 1 if previously unset
      $job_id = "$($base)_$($suffix)"
    } while (az batch job show --job-id $job_id)
  }
  $props["id"] = $job_id
  AzBatchCreate "job" $props
  return $job_id
}

# This is a workaround for what appears to be a bug in Azure CLI, that SASs containing '//'
# become malformed when put into URLs (the // becomes / and the signature is then too short)
function GenerateSAS {
  Param(
    [string]$what, # Typically blob or container
    [Parameter(mandatory=$True, ValueFromRemainingArguments=$True)]$rest
  )
  $expiry=(Get-Date).AddDays($DAYS_TO_LIVE)
  while ($True) {
    $sas=az storage $what generate-sas --expiry (Get-Date $expiry -uformat '+%Y-%m-%dT%H:%MZ') @rest
    if ($sas.IndexOf("//") -eq -1) { return $sas }
    log "Regenerating SAS as got malformed SAS $($sas) from flags $what $EXPIRY $rest"
    $expiry = $expiry.AddMinutes(1)
  }
}

function CreatePool {
  Param(
    [string]$name,
    [Parameter(mandatory=$False)]$overrides=@{}
  )
  if (az batch pool show --pool-id $name) {
    log "Found existing pool $($name), reusing"
  } else {
    $props = @{
      "id" = $name
      "virtualMachineConfiguration" = @{
        "imageReference" = @{
          "publisher" = "microsoft-azure-batch" # Canonical
          "offer" = "ubuntu-server-container" # UbuntuServer
          "sku" = "16-04-lts" # 18.04-LTS
        }
        "nodeAgentSKUId" = "batch.node.ubuntu 16.04"
        "containerConfiguration" = @{}
      }
      "vm_size" = "STANDARD_NC6S_V2"
      # Never more than 70 nodes (==GPUs, 70*6=420 CPU cores); resize down to 0 only after being idle for last 30 minutes
      "enableAutoScale" = $True
      "autoScaleFormula" = '$TargetLowPriorityNodes = min(70, max(0, $PendingTasks.GetSample(TimeInterval_Minute*30, 0)))'
      "autoScaleEvaluationInterval" = "PT5M"
      # Azure Security Pack updates, as per Andy.Slowey@
      "applicationPackageReferences" = @(
        @{
          "applicationId" = "installazsecpack"
        }
      )
      "startTask" = @{
        "commandLine" = "/bin/bash -c '`$AZ_BATCH_APP_PACKAGE_installazsecpack/InstallAzSecPack.sh -i `"$((az account show | ConvertFrom-Json).id)`"'"
        "userIdentity" = @{
          "autoUser" = @{
            "elevationLevel" = "admin"
            "scope" = "pool"
          }
        }
        "waitForSuccess" = $False
      }
    }
    $overrides.GetEnumerator() |% {$props[$_.Key] = $_.Value}
    AzBatchCreate "pool" $props
  }
}

log Creating src.zip
git archive --format=zip --output=src.zip HEAD .\rlo\src\ .\rlo\test\ .\rlo\datasets\
# git archive doesn't include the contents of the submodule. Add the files ourselves.
Add-Type -Assembly System.IO.Compression.FileSystem
$zip = [System.IO.Compression.ZipFile]::Open("src.zip", "update")
Get-Childitem -Recurse -File .\src\python | Resolve-Path -Relative |% {
  [void]([System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $_, $_.Replace("\", "/"), [System.IO.Compression.CompressionLevel]::Optimal))
}
$zip.Dispose()

log Uploading src.zip to Blob Storage
$env:AZURE_STORAGE_ACCOUNT="knossosbuildpipeline"
az storage container create --name "sources" > $null
$SRC_NAME="src_$($env:BUILD_SOURCEVERSION).zip"
az storage blob upload --container-name "sources" --name $SRC_NAME --file "src.zip"
$SRC_SAS=GenerateSAS blob --name $SRC_NAME --container-name "sources" --permission "r"
$SRC_URL=az storage blob url --container-name "sources" --name $SRC_NAME --sas-token $SRC_SAS | ConvertFrom-Json
log src_url $SRC_URL
$srcZipResource = @{
  "httpUrl" = $SRC_URL
  "filePath"="src.zip"
}

az storage container create --name "results" > $null
$RESULTS_SAS=GenerateSAS container --name "results" --permissions "rwl"
$RESULTS_SAS_URL=az storage blob url --container-name "results" --name $BUILD --sas-token $RESULTS_SAS | ConvertFrom-Json

az storage container create --name "verbose" > $null
$VERBOSE_SAS=GenerateSAS container --name "verbose" --permissions "rwl"
$VERBOSE_SAS_URL=az storage blob url --container-name "verbose" --name $BUILD --sas-token $VERBOSE_SAS | ConvertFrom-Json

$containerSettings = @{
  # The tag here must match that of docker_tag.sh
  "imageName" = "knossos.azurecr.io/rlo_linux_base:$(git rev-parse --short=10 $(git hash-object rlo/test/builds/Docker/Dockerfile))$(git rev-parse --short=10 $(git hash-object rlo/test/builds/conda-env.yaml))"
  "registry" = @{
    "registryServer" = "knossos.azurecr.io"
    "username" = "knossos"
    # $env:DOCKER_PASSWORD should be passed in from the build system
    "password" = $env:DOCKER_PASSWORD
  }
  "containerRunOptions" = "--shm-size=1g --ulimit memlock=-1 --ulimit stack=67108864"
}

function OutputUploader {
  Param($filePattern, $sasUrl)
  @{
    "filePattern" = $filePattern
    "uploadOptions" = @{"uploadCondition"= "taskcompletion"}
    "destination" = @{
      "container" = @{
        "containerUrl" = $sasUrl
      }
    }
  }
}

function StdErrUploader {
  Param([string]$subfolder)
  @{
    "filePattern" = "../std*.txt"
    "uploadOptions" = @{"uploadCondition" = "TaskCompletion"}
    "destination" = @{
      "container"= @{
        "containerUrl" = az storage blob url --container-name "results" --name "$($BUILD)/$($subfolder)" --sas-token $RESULTS_SAS | ConvertFrom-Json
      }
    }
  }
}

function CreateDockerSrcTask {
  Param($cmds, $props, $job=$BUILD)
  # TF_DETERMINISTIC_OPS here is exposed by the NVidia NGC Docker container.
  $props["commandLine"] = "/bin/bash -c 'unzip src.zip; sh ./rlo/test/builds/free_memory.sh & python3 rlo/src/rlo/diagnostics.py; export TF_DETERMINISTIC_OPS=1; $cmds'"
  $props["containerSettings"] = $containerSettings
  $props["resourceFiles"] = @( $srcZipResource )
  AzBatchCreate "task" $props --job-id $job
}


# We use explicit 'az storage blob download-batch' commands because this provides more flexible wildcard matching
# than Azure Batch Task ResourceFile (storageContainerUrl + blobPrefix), specifically allowing
# us to download only the small events.json files rather than the much-bigger events_all.json files.
function DownloadCmd {
  Param($pattern, $container="results", $sas=$RESULTS_SAS)
  # The setting of $HOME prevents the CLI tool trying to store its settings in ~/.azure.
  "HOME=`$AZ_BATCH_TASK_WORKING_DIR az storage blob download-batch --source $($container) --destination . --pattern '$($pattern)' --account-name $($env:AZURE_STORAGE_ACCOUNT) --sas-token $($sas)"
}

function WaitForJobCompletion {
  Param( [Parameter(Mandatory=$False)][string[]]$job_ids=@($BUILD) )
  # Keep the node-id on which each task was last running, keyed by jobid/taskid
  $last_running = @{}
  $last_msg_time = (Get-Date).AddHours(-1) # So loop below thinks it's time to display a new message
  log "Waiting for Azure Batch job(s) $($job_ids) to complete..."
  while ($True) {
    $states = $job_ids |% {(az batch job show --job-id $_ | ConvertFrom-Json).state}
    $state_timestamp = Get-Date
    $summary = "$($state_timestamp) job(s) now: $($states)"
    if ($states.contains("completed")) {
      log $summary
      break
    }
    $msgs = @()
    $changed = $False
    $job_ids |% {
      $job_id = $_
      (az batch task list --job-id $job_id | ConvertFrom-Json) |% {
        $task = $_
        $key = "$($job_id)/$($task.id)"
        # If the condition is false, this evaluates to no results, i.e. node_id becomes $null
        $node_id = if (($task.state -eq "running") -and ($task.nodeInfo -ne $null)) {$task.nodeInfo.nodeId}
        # Powershell dictionary [lookup] is like python dict .get(); removal is by assigning null.
        if ($last_running[$key] -ne $node_id) {
          $changed = $True
          if ($node_id -eq $null) {
            $msgs += "$($key) stopped running, last seen on $($last_running[$key])"
          } else {
            # Might this throw? If the node had been deallocated
            # after we found the task had started running on it (az batch task list, above)
            # but before we try to get the IP address of the node here?
            $ip = (az batch node show --pool-id $task.nodeInfo.poolId --node-id $task.nodeInfo.nodeId | ConvertFrom-Json).ipAddress
            $msgs += "$($key) started running on $($node_id) @ $($ip)"
          }
          $last_running[$key] = $node_id
        } else {
          $node = $last_running[$key]
          $msg = $(if ($node -eq $null) { "not running on any node" } else { "running on $($node)" })
          $msgs += "  ($($key) still $($msg) )"
        }
      }
    }
    if ($changed -or (($state_timestamp - $last_msg_time).TotalMinutes -gt 10)) {
      log $summary
      $last_msg_time = $state_timestamp
    }
    if ($changed) { $msgs | Write-Host }
    sleep 60
  }
}

function CheckExitCodes {
  Param($tasks)
  # Look for task failures. The ideal here would be to identify which (if any) failed intrinsically
  # (excluding those which were terminated by Azure Batch after another task failed), and output
  # their stderr (or just the end thereof) here. (Even better, stderr retrieved from the node.)
  log Looking for task failures
  $ANY_FAILED = $false
  $tasks |% {
    $TASK = $_
    $EXITCODE = $TASK.executionInfo.exitCode
    log Task $TASK.id exitcode $EXITCODE
    if (($EXITCODE -ne $null) -and ($EXITCODE -ne 0)) { $ANY_FAILED = $True }
    $FAILURE_REASON=$TASK.executionInfo.failureInfo
    if ($FAILURE_REASON -ne $null) {
        log ...failed because $FAILURE_REASON
        $ANY_FAILED = $true
    }
  }
  return $ANY_FAILED
}

function PrintTotalTime {
  Param($tasks)
  $taskTimes = $tasks |% { try {[DateTime]::Parse($_.executionInfo.endTime) - [DateTime]::Parse($_.executionInfo.startTime)} catch {} }
  $totalTime = 0
  $taskTimes |% {$totalTime += $_}
  log "From $($TASKS.Length) tasks, got $($taskTimes.Length) durations"
  log "Total: $([System.Math]::Round($totalTime.TotalHours, 3)) hours == $([System.Math]::Round($totalTime.TotalSeconds)) seconds"
}

function CheckTasksDisplayTime {
  Param( [Parameter(Mandatory=$False)][string]$job=$BUILD )
  $tasks = (az batch task list --job-id $job | ConvertFrom-Json)
  $ANY_FAILED = CheckExitCodes $tasks
  PrintTotalTime $tasks
  return $ANY_FAILED
}
