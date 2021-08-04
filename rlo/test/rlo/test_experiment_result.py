import shutil
import pytest
from rlo.experiment_result import load_config_events
from rlo.reporting import azureml


def check_load_config_events(run_id, verbosity, expected_num_events, *args, **kwargs):
    config, events = load_config_events(run_id, *args, **kwargs, verbosity=verbosity)
    # Read the cached version
    config2, events2 = load_config_events(run_id, verbosity=verbosity)
    assert config == config2
    events = list(events)
    events2 = list(events2)
    assert len(events) == len(events2) == expected_num_events
    for ev, ev2 in zip(events, events2):
        assert ev == ev2
    shutil.rmtree(f"outputs/Run_{run_id}")


@pytest.mark.azurecli
@pytest.mark.parametrize("verbosity,expected_num_events", [(0, 190), (1, 758)])
def test_download_azureml(verbosity, expected_num_events):
    run_id = "2021-01-07-17-24-15_fewer_simplify_rules"
    azureml_run_id = "790d0a99-2172-40f3-b168-f01dcb89716a"
    check_load_config_events(
        run_id,
        source="azureml",
        workspace_name="knossosws",
        azureml_run_id=azureml_run_id,
        verbosity=verbosity,
        expected_num_events=expected_num_events,
    )


@pytest.mark.azurecli
@pytest.mark.parametrize("verbosity,expected_num_events", [(0, 190), (1, 758)])
def test_download_azurebatch(verbosity, expected_num_events):
    run_id = "fewer_simplify_rules_20210108-7"
    check_load_config_events(
        run_id,
        source="azurebatch",
        verbosity=verbosity,
        expected_num_events=expected_num_events,
    )


@pytest.mark.azurecli
@pytest.mark.parametrize(
    "run_id",
    [
        "2021-06-25-17-34-57_6227_fewer_simplify_rules",
        "2021-07-08-14-35-15_473_summations",
    ],
)
def test_model_download(run_id):
    azureml.download_model_state(run_id, 0, allow_interactive=True)
