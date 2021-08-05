Param(
    [Parameter(Mandatory=$True)][int]$num_nodes,
    [Parameter(Mandatory=$False)][int]$workers_per_node=1,
    [Parameter(Mandatory=$False)][Int]$DAYS_TO_LIVE=1,
    [Parameter(Mandatory=$False)][Int]$test_kill_worker=-1,
    [Parameter(Position=1, Mandatory=$True)][string]$scenario,
    [Parameter(Position=2, ValueFromRemainingArguments=$True, Mandatory=$False)][string[]]$other_flags=@()
)

# The ID of the main job, used by some functions in azure_batch_common
$BUILD = "build-ray-$($env:BUILD_BUILDNUMBER)"
$redis_port = 53891
$passwd = $env:BUILD_BUILDNUMBER # Must be the same for workers / driver to connect to head
Import-Module "$PSScriptRoot\azure_batch_common.ps1"

# 0. Set gpu_memory_fraction if not done explicitly. Avoids one of the most common errors.
if (($workers_per_node -gt 1) -and !$other_flags.Contains("--gpu_memory_fraction")) {
  # Known-good values for 2 or 3 workers_per_node.
  if ($workers_per_node -gt 3) {
    throw "No gpu_memory_fraction supplied and no known-good default for $($workers_per_node) workers_per_node>3"
  }
  $frac = @(0.45, 0.3)[$workers_per_node - 2]
  $other_flags = "--gpu_memory_fraction=$($frac) $($other_flags)"
}

# 1. Headnode pool of dedicated nodes with CPU only
$head_pool = "knossos-ray-cpu-head"
CreatePool $head_pool @{
  "networkConfiguration" = @{
      # Note the subgroup here must either (1) have no Network Security Group - this is what we've done, the ray head node
      # sits within a different subnet (with restrictive NSG), but both subnets within the same VNET so nodes can talk to head;
      # or (2) have an NSG that allows inbound traffic from service tag "BatchNodeManagement" to ports 29876-29877
      # as per https://docs.microsoft.com/en-us/azure/batch/batch-virtual-network
      "subnetId" = "/subscriptions/0f64a630-b912-4501-ad57-262a9e12637c/resourceGroups/adobuilds/providers/Microsoft.Network/virtualNetworks/adobuilds-vnet/subnets/batch"
    }
    "enableInterNodeCommunication" = $True # Do we need this??
    "vm_size" = "STANDARD_D8_V3" # 32GB RAM
    # CPU machines are relatively cheap - so keep them around for 90mins to help get runs started faster
    "autoScaleFormula" = '$TargetDedicatedNodes = max(0, $PendingTasks.GetSample(TimeInterval_Minute*90, 0))'
}

# 2. Worker pool. Not specific to any job/build/headnode.
$worker_pool = "knossos-gpu-ray-workers"
CreatePool $worker_pool @{
  "networkConfiguration" = @{
      # Note the subgroup here must either (1) have no Network Security Group - this is what we've done, the ray head node
      # sits within a different subnet (with restrictive NSG), but both subnets within the same VNET so nodes can talk to head;
      # or (2) have an NSG that allows inbound traffic from service tag "BatchNodeManagement" to ports 29876-29877
      # as per https://docs.microsoft.com/en-us/azure/batch/batch-virtual-network
      "subnetId" = "/subscriptions/0f64a630-b912-4501-ad57-262a9e12637c/resourceGroups/adobuilds/providers/Microsoft.Network/virtualNetworks/adobuilds-vnet/subnets/batch"
    }
    "enableInterNodeCommunication" = $True
    "autoScaleFormula" = '$TargetLowPriorityNodes = min(60, max(0, $PendingTasks.GetSample(TimeInterval_Minute*30, 0)))'
}

# This modifies the container settings used by CreateDockerSrcTask.
# The option tells Docker that connections to/from the containers, should be to/from the same IP address as the host.
# This is necessary because our VPN subnet NSG only allows (Ray) network connections between the two Batch pools if they have the correct IP addresses.
$containerSettings["containerRunOptions"] = "$($containerSettings["containerRunOptions"]) --network host"

# 3. Run headnode job. If we have only one GPU machine, run the head on that
# (so it will run the Ray/redis server, our python "driver"/controller, *and* GPU Workers),
# otherwise run the head (Ray/redis server) together with the GPU-less python controller in the head pool,
# and bodies (GPUs) separately (below)
$BUILD = CreateJob $BUILD @{
  "poolInfo" = @{ "poolId" = $(if ($num_nodes -gt 1) {$head_pool} else {$worker_pool}) }
  "onTaskFailure" = "performexitoptionsjobaction" # Can't be specified by command-line, hence json. Hopefully will give us decent defaults for task ExitOptions.
}

if ($test_kill_worker -gt -1) {
  if ($num_nodes -lt 2) {
    throw "Must have multiple nodes to test killing workers"
  }
  $other_flags = "--test_kill_worker_after_tasks=$($test_kill_worker) $($other_flags)"
}

CreateDockerSrcTask ("export PYTHONPATH=`$(pwd)/src/;" +
      " ray start --head --redis-port $($redis_port) --redis-password $($passwd)" +
      " && python src/ray_main.py $scenario $($other_flags) --address 127.0.0.1:$($redis_port) --redis_token $($passwd)" +
      " --workers_per_gpu=$($workers_per_node) --run_id $($scenario)_ray-$($env:BUILD_BUILDNUMBER) --gitlog $(git rev-parse --short HEAD) --force_gpu") @{
  "id" = "head"
  "outputFiles" = @(
      (StdErrUploader "head/"),
      # Do not upload ray_timeline.json, ray_object_transfers.json to outputs - these can be enormous
      (OutputUploader "outputs/Run*/config.json" $RESULTS_SAS_URL)
      (OutputUploader "outputs/**/events*.json.bz2" $RESULTS_SAS_URL)
      (OutputUploader "outputs/**/train.pck" $RESULTS_SAS_URL),
      (OutputUploader "outputs/**/*.png" $RESULTS_SAS_URL),
      (OutputUploader "outputs/**/*.html" $RESULTS_SAS_URL),
      (OutputUploader "outputs/**/*.kso" $RESULTS_SAS_URL),
      (OutputUploader "outputs/**/*.mp4" $RESULTS_SAS_URL),
      # Upload everything to verbose.
      (OutputUploader "outputs/**/*" $VERBOSE_SAS_URL)
    )
}

az batch job set --job-id $BUILD --on-all-tasks-complete "terminatejob"
$jobs = @($BUILD)

# If there are no other nodes, that's all we need to do, the head will run on its own.
# If there are other workers, the headnode will sit waiting for them to connect, so start them pointing at the headnode's IP address.
if ($num_nodes -gt 1) {
  # 4. Get the headnode IP
  while ($True) {
    Write-Host "Waiting for headnode to start..."
    $task = az batch task show --job-id $BUILD --task-id "head"
    $nodeInfo = ($task | ConvertFrom-Json).nodeInfo
    if ($nodeInfo -ne $null) {
      $headnode_ip = (az batch node show --pool-id $head_pool --node-id $nodeInfo.nodeId | ConvertFrom-Json).ipAddress
      Write-Host "Headnode IP $($headnode_ip)"
      break
    }
    sleep 30
  }

  # 5. Run tasks for the workers. We call these "bodies" because each merely connects to the "head" (redis server)
  # which can then instantiate actors (python Worker objects) on them. Note there is no connection between the index
  # of the Worker object and the number of the Azure Batch task (body); our python merely requests the Worker objects
  # be created and Ray puts them on *some* machine with appropriate resources (i.e. a body).
  CreateJob -force_name "$($BUILD)-bodies" @{
    "poolInfo" = @{ "poolId" = $worker_pool }
    # For e.g. Azure Batch Graphs, we make any failing task terminate the whole job, like this:
    # "onTaskFailure" = "performexitoptionsjobaction"
    # However, here if one Ray worker can't connect, there's no reason not to let the others try;
    # bugs in our python code, even executing on the workers, are reported on the *headnode*.
  }

  $ray_cmd = "ray start --address $($headnode_ip):$($redis_port) --redis-password $($passwd) --block"
  # Start counting bodies at 0 as the headnode is not one (unless num_nodes == 1)
  for ($w = 0; $w -lt $num_nodes; $w++) {
    # Retry a few times in case we can't connect.
    # (The workers are started as soon as the headnode has an IP,
    # but the headnode still has to download sources etc. before we can connect)
    CreateDockerSrcTask "export PYTHONPATH=`$(pwd)/src; for retry in 0 1 2 3 4 5; do $ray_cmd && break; sleep 60; echo Retrying...; done" @{
      "id" = "body$($w)"
      "outputFiles" = @(
        StdErrUploader "$($w)/"
      )
    } "$($BUILD)-bodies"
  }
  # Later, we'll want to handle failing workers gracefully and continue. In the meantime, kill everything if a worker dies.
  az batch job set --job-id "$($BUILD)-bodies" --on-all-tasks-complete "terminatejob"
  $jobs += "$($BUILD)-bodies"
}

WaitForJobCompletion $jobs
# Stop all jobs if either has failed.
$jobs |% {
  Write-Host "Stopping job $_"
  az batch job stop --job-id $_
}

# Download results onto local machine in order to publish (in a later build step)
[void](mkdir results)

Write-Host Downloading into results/...
# Also .html? We don't have any at the moment so that'd mark as failed here.
@("config.json", "train.pck", "*.png", "*.kso", "*.mp4") |% {
  $r = az storage blob download-batch --destination "./results" --source "results" --no-progress --pattern "$($BUILD)/**/$_"
  Write-Host $r
  # If az "succeeds", it returns a json string list of files downloaded
  if ((! $r) -or ($r | ConvertFrom-Json).Count -eq 0) {
    $ANY_FAILED = $True
  }
}

#Annoyingly that puts everything in results/$BUILD
try {
  mv results/$BUILD/* results/ -ErrorAction Stop
  [void](rmdir results/$BUILD -ErrorAction Stop)
} catch {
  Write-Host "No results found"
  $ANY_FAILED = $True
}

if (CheckExitCodes (az batch task list --job-id $BUILD | ConvertFrom-Json)) { $ANY_FAILED = $True }
PrintTotalTime ($jobs |% {az batch task list --job-id $_ | ConvertFrom-Json})

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
