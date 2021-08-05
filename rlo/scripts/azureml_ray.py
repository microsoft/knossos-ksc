"""
Quick start instructions:

1. Make sure you have access to the knossos Azure subscription.
    > az login
    > az account set -s Knossos
  You'll need to have at least Contributor access to the resource group (knossosamlrg).
  You can (not recommended) temporarily elevate yourself to Contributor using Azure
   Privileged Identity Management (PIM), but you will need to have said level of access
   _for_the_duration_of_the_run_ not just when it starts, or the run will fail to complete.
  Alternatively (recommended), you can temporarily elevate yourself to Owner using PIM, and
     give yourself permanent Contributor rights to the knossosaml resource group.

2. Install dependencies (better to do it in an isolated conda environment because it may clash)

```
pip install -r test/builds/ci-requirements.txt
```

3. Try to run `fewer_simplify_rules` scenario

```
python scripts/azureml_ray.py fewer_simplify_rules --num_workers=2
```

Note: any additional arguments will be passed to `ray_main.py`.
"""

import argparse
from datetime import datetime
import os
import subprocess
import sys
from tempfile import NamedTemporaryFile

from azureml.core import Workspace, Experiment, Environment
from azureml.core.authentication import AzureCliAuthentication
from azureml.pipeline.core import PipelineRun

# Azure ML Reinforcement Learning imports
from azureml.contrib.train.rl import ReinforcementLearningEstimator, Ray
from azureml.contrib.train.rl import WorkerConfiguration

from azureml_util import combined_source_directory

sys.path.append(os.path.join(os.path.dirname(__file__), "../src"))
from rlo import git_utils


def get_environment():
    print("Getting docker password...")
    password = (
        subprocess.check_output(["bash", "./test/builds/docker_password.sh"])
        .decode("ASCII")
        .strip()
    )
    docker_tag = (
        subprocess.check_output(["bash", "./test/builds/docker_tag.sh"])
        .decode("ASCII")
        .strip()
    )
    print("Docker tag", docker_tag)
    env = Environment("my_ray_env")
    # ws.register(env) - no, let's not
    env.docker.base_image = "rlo_linux_base:" + docker_tag
    env.docker.base_image_registry.address = "knossos.azurecr.io"
    env.docker.base_image_registry.password = password
    env.docker.base_image_registry.username = "00000000-0000-0000-0000-000000000000"
    env.python.user_managed_dependencies = True
    return env


def check_params(scenario, params):
    # import locally so that CI can run without dependencies
    from rlo.flags import make_config_for_scenario
    from ray_main import ray_run_arguments

    if not os.path.isfile(scenario):
        scenario = os.path.join("src", "scenarios", f"{scenario}.json")
    make_config_for_scenario(scenario, ray_run_arguments, cmdline=params)


def check_best_episodes(run):
    """ Checks best_episodes.kso file does not contain NaNs
        Regression test for #1438
    """
    head_run = next(rn for rn in run.get_children() if rn.id.endswith("_head"))
    kso_file = next(
        f for f in head_run.get_file_names() if f.endswith("best_episodes.kso")
    )
    with NamedTemporaryFile(mode="r") as f:
        head_run.download_file(kso_file, output_file_path=f.name)
        content = f.read()
    print(f"Downloaded {kso_file} (length={len(content)})")
    if "NaN" in content:
        lines = [line for line in content.split("\n") if "NaN" in line]
        raise ValueError(
            "Expected no NaNs, but found the following lines:\n" + "\n".join(lines)
        )
    print("Found no NaNs!")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("scenario", type=str, help="Scenario to run")
    parser.add_argument(
        "--aml_config_file",
        type=str,
        default="aml_config.json",
        help="Path to the Azure ML config file",
    )
    parser.add_argument(
        "--head_cluster",
        type=str,
        default="ray-head",
        help="Name of the headnode cluster",
    )
    parser.add_argument(
        "--body_cluster",
        type=str,
        default="ray-p40",
        help="Name of the worker cluster",
    )
    parser.add_argument(
        "--num_workers", type=int, default=10, help="Number of worker nodes",
    )
    parser.add_argument(
        "--enable_cli_auth",
        action="store_true",
        help="Enable AzureCliAuthentication (for CI)",
    )
    parser.add_argument(
        "--wait_for_completion",
        action="store_true",
        help="Wait for the completion of the run",
    )
    parser.add_argument(
        "--no-check-params",
        action="store_false",
        dest="check_params",
        help="Don't check if the parameters are valid locally before submission",
    )
    args, additional_args = parser.parse_known_args()

    run_id = "{}_{}".format(datetime.now().strftime("%Y-%m-%d-%H-%M-%S"), args.scenario)
    params = [
        args.scenario,
        "--run_id",
        run_id,
        "--gitlog",
        git_utils.get_git_revision_short_hash(),
    ] + additional_args

    # Check arguments are valid
    if args.check_params:
        check_params(args.scenario, params)

    if args.enable_cli_auth:
        cli_auth = AzureCliAuthentication()
    else:
        cli_auth = None
    ws = Workspace.from_config(args.aml_config_file, auth=cli_auth)
    head_compute = ws.compute_targets[args.head_cluster]
    body_compute = ws.compute_targets[args.body_cluster]

    env = get_environment()

    # Specify the Ray worker configuration
    worker_conf = WorkerConfiguration(
        compute_target=body_compute,
        node_count=args.num_workers,
        use_gpu=True,
        environment=env,
    )

    with combined_source_directory() as source_dir:
        # This defines the head configuration - note, no GPU required, the head is expected to be only a co-ordinator.
        est = ReinforcementLearningEstimator(
            source_directory=source_dir,
            compute_target=head_compute,
            environment=env,
            entry_script="ray_main.py",
            script_params=dict(
                [(p, "") for p in params]
                + [
                    # This tells ray_main.py to connect to an existing Ray/redis server rather than start its own
                    ("--address", "auto"),
                    # The command-line ray process run by the AzureML RL framework seems to default to this.
                    # This can be seen by "pip install ray==0.8.7; ray start --head".
                    ("--redis_token", "5241590000000000"),
                    # Ensure workers are using GPUs
                    ("--force_gpu", ""),
                ]
            ),
            rl_framework=Ray(version="0.8.3"),
            worker_configuration=worker_conf,
            use_gpu=False,
            cluster_coordination_timeout_seconds=3600,  # How long to wait for whole cluster to start
            max_run_duration_seconds=40 * 3600,
            # Allow the docker container Ray runs in to make full use
            # of the shared memory available from the host OS.
            shm_size=24 * 1024 * 1024 * 1024,
        )

        # This runs the estimator, but doesn't do anything to save the outputs (PNGs or events.json)
        experiment = Experiment(ws, args.scenario)
        run = experiment.submit(config=est)
    print(run)
    print("Run submitted!", run.get_portal_url())
    if args.wait_for_completion:
        run.wait_for_completion()

        # Check no NaN in best_episodes.kso
        check_best_episodes(run)

        print("Run succeeded!")
        print(
            "Download from container azureml path ExperimentRun/dcid.{run_id}/outputs"
        )
        print("Note: run_id is the ID of the head step:")
        pipeline_run = PipelineRun(experiment, run.id)
        child_ids = [child_run.id for child_run in pipeline_run.get_steps()]
        print([id for id in child_ids if "head" in id])


if __name__ == "__main__":
    main()
