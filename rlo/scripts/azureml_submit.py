"""
Quick start instructions:

0. (important) make sure that the system time is correct

```
sudo ntpdate time.windows.com
```

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
python scripts/azureml_submit.py fewer_simplify_rules --num_repetitions 2
```

Note: any additional arguments will be passed to `train_over_expressions.py`.
"""

import argparse
from datetime import datetime
import sys
import os

from azureml.core import Workspace, Experiment, ScriptRunConfig, Environment
from azureml.core import Datastore
from azureml.pipeline.core import Pipeline
from azureml.pipeline.core import PipelineData
from azureml.pipeline.steps import CommandStep

# This line causes the AMLK8s targets to appear in workspace
from azureml.contrib.core.k8srunconfig import K8sComputeConfiguration

from msrest.exceptions import HttpOperationError

from azureml_util import get_base_docker_image, combined_source_directory

sys.path.append(os.path.join(os.path.dirname(__file__), "../src"))
from rlo import git_utils
from rlo.flags import make_config_for_scenario
from rlo.reporting.cosmosdb import get_remote_run_info_from_db

DOCKER_FILE_PATH = os.path.join(
    os.path.dirname(__file__), "../test/builds/Docker/Dockerfile"
)


def get_datastore(ws):
    """ Registers a storage to a workspace or retrieves it if it is already registered.
    """
    # this name is for the workspace and does not have to agree with account_name
    blob_datastore_name = "knossosbuildpipeline"

    try:
        blob_datastore = Datastore.get(ws, blob_datastore_name)
        print("Found Blob Datastore with name: %s" % blob_datastore_name)
    except HttpOperationError:
        # This is a one-off step to register the blob container with
        # the Azure ML workspace. If you need to run this step, please
        # look up the account_key in Azure portal.
        account_name = "knossosbuildpipeline"
        container_name = "aml"
        account_key = input(
            "Type in the account key for blob storage {} "
            "(this can be looked up in Azure portal)".format(account_name)
        )

        blob_datastore = Datastore.register_azure_blob_container(
            workspace=ws,
            datastore_name=blob_datastore_name,
            account_name=account_name,  # Storage account name
            container_name=container_name,  # Name of Azure blob container
            account_key=account_key,  # Storage account key
            create_if_not_exists=False,
        )
        print("Registered blob datastore with name: %s" % blob_datastore_name)
    return blob_datastore


def check_params(scenario, params):
    if not os.path.isfile(scenario):
        scenario = os.path.join("src", "scenarios", f"{scenario}.json")
    make_config_for_scenario(scenario, cmdline=params)


def get_k8sconfig():
    k8sconfig = K8sComputeConfiguration()
    k8sconfig.configuration = {"gpu_count": 1, "preemption_allowed": False}
    return k8sconfig


AMLK8S_CLUSTERS = ["itpeastusv100cl"]


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
        "--compute_cluster",
        type=str,
        default="itpeastusv100cl",
        choices=["p100cluster", "k80cluster", "cpucluster"] + AMLK8S_CLUSTERS,
        help="Name of the compute cluster",
    )
    parser.add_argument(
        "--num_repetitions",
        type=int,
        default=10,
        help="Number of repetitions (overrides scenario)",
    )
    parser.add_argument(
        "--wait_for_completion",
        action="store_true",
        help="Wait for the completion of the run",
    )
    args, additional_args = parser.parse_known_args()

    run_id = (
        f'{datetime.now().strftime("%Y-%m-%d-%H-%M-%S")}_{os.getpid()}_{args.scenario}'
    )
    params = [
        args.scenario,
        "--run_id",
        run_id,
        "--gitlog",
        git_utils.get_git_revision_short_hash(),
        "--num_repetitions",
        str(args.num_repetitions),
    ] + additional_args

    # Check arguments are valid
    check_params(args.scenario, params)

    ws = Workspace.from_config(args.aml_config_file)
    compute_target = ws.compute_targets[args.compute_cluster]

    with combined_source_directory() as source_dir:
        # Create a factory to keep common setup in one place
        conda_file = os.path.join("test", "builds", "conda-env.yaml")
        base_image = get_base_docker_image(DOCKER_FILE_PATH)
        environment = Environment.from_docker_image(
            "rlo_environment", base_image, conda_specification=conda_file,
        )
        script_config_factory = lambda: ScriptRunConfig(
            source_directory=source_dir,
            compute_target=compute_target,
            environment=environment,
        )
        script_config = script_config_factory()
        if args.compute_cluster in AMLK8S_CLUSTERS:
            # Undocumented. Learned from amlt/client/amlk8s_client.py
            script_config.run_config.cmk8scompute = get_k8sconfig()

        # Get blob storage attached to the workspace
        def_blob_store = get_datastore(ws)

        # Create repetitions; outputs are referenced by PipelineData objects in outputs_from_repetitions.
        outputs_from_repetitions = []
        for rep in range(args.num_repetitions):
            output = PipelineData(
                "output_{}".format(rep), datastore=def_blob_store, is_directory=True
            )
            CommandStep(
                name=f"Run_repetition_{rep}",
                command=["python", "train_over_expressions.py"]
                + params
                + ["--repetition", rep, "--output_dir", output],
                runconfig=script_config,
                outputs=[output],
            )
            outputs_from_repetitions.append(output)

        # Now create the plotting step; the outputs of the repetitions become the inputs here.
        plot_step = CommandStep(
            name="Plot_all_repetitions",
            command=["python", "summarize_logs.py"]
            + params
            + [
                # In AML, `outputs` is a special directory and the contents will be stored in a blob storage
                "--output_dir",
                "outputs",
                "--num_repetitions",
                args.num_repetitions,
                "--repetition_output_dirs",
            ]
            + outputs_from_repetitions,
            runconfig=script_config_factory(),
            inputs=outputs_from_repetitions,  # create data dependency
            compute_target=ws.compute_targets["cpucluster"],
        )
        # No need to schedule the repetitions because there is a data dependency
        pipeline = Pipeline(workspace=ws, steps=[plot_step])
        pipeline_run = Experiment(ws, args.scenario).submit(pipeline)
    print(pipeline_run)
    plot_id = None
    if args.wait_for_completion:
        pipeline_run.wait_for_completion()
        print("Run succeeded!")
        run_ids = [c.id for c in pipeline_run.find_step_run("Plot_all_repetitions")]
        if len(run_ids) == 1:
            plot_id = run_ids[0]
        # Check Cosmos DB has the same information
        info = get_remote_run_info_from_db(run_id)
        assert info.workspace_name == ws.name
        assert info.azureml_run_id == plot_id
    print(
        f"""To download the results, use the following command:

```python
from rlo.experiment_result import load_config_events

config, events = load_config_events('{run_id}')
```
"""
    )


if __name__ == "__main__":
    main()
