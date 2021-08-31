#!/bin/bash
set -e
set -x
export AZURE_STORAGE_ACCOUNT="knossosbuildpipeline"
plot_cmd="python3 src/plot_costs.py"
mkdir outputs
while (( "$#" )); do
  # arg is of the form scenario_name_build-number[=caption]
  caption=$(echo $1 | sed 's/^[^=]*//g')
  experiment=$(echo $1 | sed 's/=.*//g')
  build=build-$(echo $experiment | sed 's/.*_//g')
  # Wildcard here should work for both legacy Experiment_ and newer Run_ directories.
  # Whereas ...${experiment}/**/*.json (unexpectedly) fails to match the toplevel config.json,
  # without the ** it gets both config.json and (unexpectedly) _the events.json in the subfolders_.
  az storage blob download-batch --source results --destination . --pattern "${build}/*_${experiment}/"'*.json'
  mv ${build}/* outputs/
  rmdir ${build}
  plot_cmd="${plot_cmd} ${experiment}${caption}"
  shift
done
$plot_cmd
($plot_cmd --frequency_x_scale=generation --outfile_suffix=_by_generation && $plot_cmd --frequency_x_scale=total_train_time
  ) || echo Frequency plot failed, should still have cost/opt plots
