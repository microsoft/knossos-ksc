# 1. Make sure you have access to the knossos Azure subscription.
#     > az login
#     > az account set -s Knossos
#   You'll need to have at least Contributor access to the resource group (knossosamlrg).

import os

from azureml.core import ScriptRunConfig, Environment, Experiment
from azureml.core.workspace import Workspace

from azureml_util import combined_source_directory


def main():
    workspace = Workspace.from_config()
    workspace.get_details()

    experiment = Experiment(workspace, "benchmark_KSC")

    cluster = workspace.compute_targets["smallcpucluster"]

    bench_dir = os.path.dirname(os.path.abspath(__file__))
    env = Environment.from_pip_requirements(
        "benchmark_env", os.path.join(bench_dir, "requirements.txt")
    )

    # taken from .sh, can we share cleanly?
    BENCH = "pytest src/bench/ \
            --benchmark-autosave --benchmark-max-time=5.0\
            --benchmark-name=short --benchmark-sort=name --benchmark-group-by=group,func\
            --benchmark-columns=median,iqr,outliers,mean,stddev,min,max,iterations,rounds"

    sqrl_command = BENCH + " --modulepath=examples/dl-capsule/sqrl --benchmarkname=sqrl"

    with combined_source_directory() as source_dir:
        config = ScriptRunConfig(
            source_directory=source_dir,
            compute_target=cluster,
            command=[sqrl_command],
            environment=env,
        )

        script_run = experiment.submit(config)


if __name__ == "__main__":
    main()
