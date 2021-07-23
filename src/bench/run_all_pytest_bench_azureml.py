# 1. Make sure you have access to the knossos Azure subscription.
#     > az login
#     > az account set -s Knossos
#   You'll need to have at least Contributor access to the resource group (knossosamlrg).


from azureml.core import ScriptRunConfig, Environment, Experiment
from azureml.core.workspace import Workspace


workspace = Workspace.from_config()
workspace.get_details()


experiment = Experiment(workspace, "benchmark_KSC")

cluster = workspace.compute_targets["smallcpucluster"]


env = Environment.from_pip_requirements("benchmark_env", "requirements.txt")

# taken from .sh, can we share cleanly?
BENCH = "pytest src/bench/ \
        --benchmark-autosave --benchmark-max-time=5.0\
        --benchmark-name=short --benchmark-sort=name --benchmark-group-by=group,func\
        --benchmark-columns=median,iqr,outliers,mean,stddev,min,max,iterations,rounds"

sqrl_command = BENCH + "--modulepath=examples/dl-capsule/sqrl --benchmarkname=sqrl"

config = ScriptRunConfig(
    source_directory="../../",
    compute_target=cluster,
    command=[sqrl_command],
    environment=env,
)

script_run = experiment.submit(config)
