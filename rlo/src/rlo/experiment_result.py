# mypy: ignore-errors
import abc
import bz2
import glob
import json
import os
import shutil
from tqdm import tqdm
from typing import Iterable, Dict, List, Literal, Optional
import warnings

from rlo.analytics import events_filename
from rlo import blob_storage
from rlo.reporting import RemoteRunInfo
from rlo.reporting.cosmosdb import (
    check_and_upload_config_to_db,
    get_remote_run_info_from_db,
)
from rlo import utils


class LocalExperimentResult:
    def __init__(self, run_id, **kwargs):
        self._config, self._events = load_config_events_locally(run_id, **kwargs)

    def config_and_events(self):
        return self._config, self._events

    def run_id(self):
        return self._config["run_id"]


class AzureBlobExperimentResult:
    def __init__(
        self,
        subscription_name,
        account_name,
        container_name,
        blob_root,
        verbosity=0,
        fix_missing_generation=True,  # to support old logs
    ):
        self._subscription_name = subscription_name
        self._account_name = account_name
        self._container_name = container_name
        self._blob_root = blob_root
        self._verbosity = verbosity
        self._fix_missing_generation = fix_missing_generation
        self._run_id = None

    @staticmethod
    def from_knossosbuildpipeline(run_id, **kwargs):
        """ Creates AzureBlobExperimentResult for an Azure Batch experiment

        Args:
            run_id: RLO run ID (e.g., binding_simplify_astar_20200627-9)
        """
        build_id = run_id.split("_")[-1]
        return AzureBlobExperimentResult(
            subscription_name="knossos",
            account_name="knossosbuildpipeline",
            container_name="results",
            blob_root=f"build-{build_id}/Run_{run_id}",
            **kwargs,
        )

    @staticmethod
    def from_azureml(workspace_name, azureml_run_id, run_id=None, **kwargs):
        """ Creates AzureBlobExperimentResult for an Azure ML experiment

        Args:
            workspace_name: workspace name (e.g., knossosws)
            azureml_run_id: the Azure ML Run ID of the `Plot_all_repetitions`
                step for non-Ray runs and that of the child run corresponding to
                the Ray head node for Ray runs.
            run_id: RLO run ID (e.g., 2020-12-17-14-19-46_fewer_simplify_rules).
                If unspecified, it can be determined by searching in the storage
                by calling `find_blob_root` (which can be expensive).
        """
        account_name_lookup = {
            "knossosws": "knossosws1961921937",
            "resrchvc": "resrchvc4work",
        }
        if workspace_name not in account_name_lookup:
            raise ValueError(
                f"Azure ML workspace {workspace_name} is not registered. "
                "Please lookup the name of the storage account and "
                "add a case in `account_name_lookup`."
            )
        subscription_name = "knossos"
        account_name = account_name_lookup[workspace_name]
        container_name = "azureml"
        blob_prefix = f"ExperimentRun/dcid.{azureml_run_id}/outputs/Run"
        if run_id is None:
            blob_root = blob_storage.find_blob_root(
                subscription_name, account_name, container_name, blob_prefix
            )
        else:
            blob_root = f"{blob_prefix}_{run_id}"
        return AzureBlobExperimentResult(
            subscription_name, account_name, container_name, blob_root, **kwargs
        )

    def config_and_events(self):
        config = self.download_config_from_blob()
        events = ManyLazyEvents.from_blob_storage(
            self._subscription_name,
            self._account_name,
            self._container_name,
            self._blob_root,
            verbosity=self._verbosity,
            fix_missing_generation=self._fix_missing_generation,
        )
        return config, events

    def download_config_from_blob(self):
        block_blob_service = blob_storage.get_block_blob_service(
            self._subscription_name, self._account_name
        )
        return blob_storage.get_config(
            block_blob_service, self._container_name, self._blob_root
        )

    def run_id(self):
        if self._run_id is None:
            config = self.download_config_from_blob()
            self._run_id = config["run_id"]
        return self._run_id


def get_experiment_result_from_run_id(
    run_id: str, run_info: Optional[RemoteRunInfo] = None, **kwargs
) -> AzureBlobExperimentResult:
    """
    Creates AzureBlobExperimentResult from run ID and RemoteRunInfo. RemoteRunInfo
    is queried from Cosmos DB if it is not provided.

    Args:
        run_id: run ID for the experiment.
        run_info: (optional) information about where the experiment was run.

    Returns:
        result, which can be used e.g., as `result.download_config_from_blob()`.
    """
    if run_info is None:
        run_info = get_remote_run_info_from_db(run_id)
    if run_info.source == "azurebatch":
        result = AzureBlobExperimentResult.from_knossosbuildpipeline(run_id, **kwargs)
    elif run_info.source == "azureml":
        result = AzureBlobExperimentResult.from_azureml(
            run_info.workspace_name, run_info.azureml_run_id, **kwargs
        )
    else:
        raise ValueError(f"Unknown source {run_info.source}")

    # Check run_id
    config = result.download_config_from_blob()
    assert (
        config["run_id"] == run_id
    ), f"Expected run_id {run_id}, but got {config['run_id']}"

    # Check if the information is already in Cosmos DB and upload if necessary
    check_and_upload_config_to_db(config, run_info)
    return result


class LazyEvents(Iterable[Dict]):
    """
    Iterable of events that loads lazily.

    This can be iterated over multiple times, but this will result in loading from file
    multiple times. It may be faster to load small event logs into memory using
    `list(events)`.
    """

    def __init__(
        self, event_filter=lambda e: [e], fix_missing_generation: bool = False
    ):
        self._legacy_adaptor = (
            _maybe_rename_unnumbered_epoch_to_generation
            if fix_missing_generation
            else _maybe_rename_epoch_to_generation
        )
        self._event_filter = event_filter

    @abc.abstractmethod
    def _generate_lines(self):
        raise NotImplementedError("Absract method")

    def __iter__(self):
        for line in self._generate_lines():
            yield from self._event_filter(self._legacy_adaptor(json.loads(line)))


class RawTextEvents(LazyEvents):
    def __init__(self, text, **kwargs):
        super().__init__(**kwargs)
        self._text = text

    def _generate_lines(self):
        yield from self._text.splitlines()


class TextFileEvents(LazyEvents):
    def __init__(self, filename, **kwargs):
        super().__init__(**kwargs)
        self._filename = filename

    def _generate_lines(self):
        with open(self._filename) as f:
            yield from f


class Bz2FileEvents(LazyEvents):
    def __init__(self, filename, **kwargs):
        super().__init__(**kwargs)
        self._filename = filename

    def _generate_lines(self):
        with bz2.open(self._filename, "rt", encoding="utf-8") as f:
            yield from f


def events_json_wildcard(dirname, prefix="", verbosity=0):
    return os.path.join(
        dirname, "*", prefix + events_filename(verbosity, with_bz2=False)
    )


class ManyLazyEvents:
    def __init__(self, events: List[LazyEvents]):
        self._events = events

    @staticmethod
    def from_dir(dirname: str, prefix: str = "", verbosity: int = 0, **kwargs):
        wildcard = events_json_wildcard(dirname, prefix, verbosity)
        json_events = [TextFileEvents(fn, **kwargs) for fn in glob.glob(wildcard)]
        bz2_events = [
            Bz2FileEvents(fn, **kwargs) for fn in glob.glob(wildcard + ".bz2")
        ]
        if len(json_events) + len(bz2_events) == 0:
            raise FileNotFoundError(f"No log files found matching pattern {wildcard}")
        return ManyLazyEvents(json_events + bz2_events)

    @staticmethod
    def from_blob_storage(
        subscription_name: str,
        account_name: str,
        container_name,
        blob_root,
        verbosity=0,
        **kwargs,
    ):
        block_blob_service = blob_storage.get_block_blob_service(
            subscription_name, account_name
        )
        event_jsons = blob_storage.get_events_jsons(
            block_blob_service, container_name, blob_root, verbosity
        )
        return ManyLazyEvents([RawTextEvents(evs, **kwargs) for evs in event_jsons])

    def __iter__(self):
        for events in self._events:
            yield from iter(events)


def _load_config(config_path: str) -> Dict:
    with open(config_path) as f:
        config = json.load(f)
    if "experiment_ind" in config:
        # Legacy config, there is model_save_path, and paths have {} in them
        for k in ["result_save_path", "model_save_path"]:
            config[k] = config[k].format(config["experiment_ind"])
    return config


def load_config(run_id: str) -> Dict:
    """ run_id may be an actual run ID such as binding_simplify_astar_20200627-9, or path to a config.json file. """

    def get_config_path():
        if run_id.endswith(".json"):
            filename = run_id
        else:
            filename = f"./outputs/Run_{run_id}/config.json"
        if os.path.isfile(filename):
            return filename
        raise FileNotFoundError(f"Could not identify run {run_id}")

    config_path = get_config_path()
    return _load_config(config_path)


def save_config(config, overwrite=False):
    config_path = os.path.join(config["result_save_path"], "config.json")
    if os.path.exists(config_path):
        # Check if the config is identical
        existing_config = _load_config(config_path)
        if config == existing_config:
            return
        if not overwrite:
            raise FileExistsError(f"File {config_path} already exists")
    with utils.open_file_mkdir(config_path, "w") as f:
        json.dump(config, f, sort_keys=True, indent=2)


class NoEpochInLegacyRecord(Exception):
    """ Exception raised when a "rollout_end" or "expr_value_info" log record without "epoch" field is found.
    """


def load_events_from_dir(
    dirname: str,
    event_filter=lambda e: [e],
    prefix: str = "",
    verbosity: int = 0,
    fix_missing_generation: bool = False,
) -> ManyLazyEvents:
    return ManyLazyEvents.from_dir(
        dirname=dirname,
        event_filter=event_filter,
        prefix=prefix,
        verbosity=verbosity,
        fix_missing_generation=fix_missing_generation,
    )


def save_events_to_dir(dirname, events, verbosity=0, overwrite=False):
    wildcard = events_json_wildcard(dirname, verbosity=verbosity)
    matched = glob.glob(wildcard) + glob.glob(wildcard + ".bz2")
    if len(matched) > 0 and not overwrite:
        raise FileExistsError(f"In {dirname}, found files {matched}")
    # delete existing files
    for subdirname in set([os.path.dirname(fn) for fn in matched]):
        shutil.rmtree(subdirname)
    dirname = os.path.join(dirname, "cached")
    if not os.path.exists(dirname):
        os.makedirs(dirname)
    fname = os.path.join(dirname, events_filename(verbosity, with_bz2=True))
    with bz2.open(fname, "wt", encoding="utf-8") as f:
        for event in tqdm(events, desc="Saving", unit=" events"):
            f.write(json.dumps(event) + "\n")


def load_events_from_config(config, *args, **kwargs):
    return load_events_from_dir(config["result_save_path"], *args, **kwargs)


def load_config_events_locally(run_id, **kwargs):
    """ Try to load config and events from a run_id.

    Args:
        run_id: a run_id can be
        (1) Run ID (e.g., 2019_01_06_13_15_48_13172),
        (2) Other formats accepted by load_config, or
        (3) path to a directory containing rep/events.json files.

    kwargs may contain
        verbosity: maximum verbosity (default: 0)
        event_filter: a function that yields a potentially modified
            record (see process_event filter in plot_dataset_summary.py)
        prefix: prefix for the events.json file containing the records
        fix_missing_generation: True to load old-style generation indexing
            (no 'generation' for untrained evaluation and generation=0
             for the first training and evaluation) (default: False)

    Returns:
        config, events
    """
    if os.path.isdir(run_id):
        events = load_events_from_dir(run_id, **kwargs)
        config_path = os.path.join(run_id, "config.json")
        if os.path.isfile(config_path):
            config = load_config(config_path)
            # update result_save_path to the directory
            # that contains config.json
            config["result_save_path"] = run_id
        else:
            config = None
    else:
        # Run ID and other formats accepted by load_config
        config = load_config(run_id)
        events = load_events_from_config(config, **kwargs)
    return config, events


def load_config_events(
    run_id: str,
    source: Optional[Literal["azurebatch", "azureml", "local"]] = None,
    workspace_name: Optional[str] = None,
    azureml_run_id: Optional[str] = None,
    **kwargs,
):
    """ Try to load config and events locally. If this fails, try to
        download the result from Blob storage using source and proto_run_id.

    Args:
        run_id: unique run id. For example, "summations_20200628-7"
        source: optional. "azurebatch" or "azureml"
        workspace_name: required if source is "azureml"
        azureml_run_id: required if source is "azureml". This
            is the Azure ML Run ID of the `Plot_all_repetitions` step for non-Ray runs
            (e.g., 366465c5-1113-4eac-91b5-40687d304e2e) and the Run ID of the child
            run corresponding to the Ray head node for Ray runs
            (e.g., blas_1607085396_cf52f44a_head).
    """
    try:
        return load_config_events_locally(run_id, **kwargs)
    except FileNotFoundError:
        pass
    try:
        info = RemoteRunInfo(source, workspace_name, azureml_run_id) if source else None
        result = get_experiment_result_from_run_id(run_id, info, **kwargs)
    except Exception as e:
        raise FileNotFoundError(
            f'Could not find run {run_id} locally. Please provide source, which can be "azurebatch" or "azureml".'
        ) from e

    # Download from blob storage
    config, events = result.config_and_events()

    # Cache to local file system
    config["output_dir"] = "outputs"
    config["result_save_path"] = os.path.join(
        config["output_dir"], "Run_" + config["run_id"]
    )
    try:
        save_config(config, overwrite=False)
        save_events_to_dir(
            config["result_save_path"], events, result._verbosity, overwrite=False
        )
    except:
        warnings.warn(
            f"Could not save events and config to {config['result_save_path']}!"
        )
    return config, events


def _maybe_rename_epoch_to_generation(event):
    """
    Event map function to accommodate PR #1081, ~July 2020.

    This is compatible with versions using "epoch" (legacy) and "generation" (current),
    where the epoch-numbering scheme starts at 0 for the untrained model.

    This is a pragmatic approach to short-term log back-compatibility. It is called on
    all logged events if fix_missing_epoch is False. It can be removed in the future
    if/when we no longer need to read logs from runs before #1081.
    """
    if "generation" in event:
        assert "epoch" not in event
    elif "epoch" in event:
        warnings.warn(
            'replacing "epoch" in event with "generation". '
            "This behaviour will be removed soon",
            DeprecationWarning,
        )
        event["generation"] = event.pop("epoch")
    elif event["event"] in ["rollout_end", "expr_value_info"]:
        raise NoEpochInLegacyRecord(
            f'Found the following event without "epoch" or "generation" field:\n{event}\n\n'
            "Try using fix_missing_generation flag."
        )
    return event


def _maybe_rename_unnumbered_epoch_to_generation(event):
    """
    Event map function to accommodate PR #1081, ~July 2020.

    This is compatible with old logs from before PR #1046, where events were
    given an "epoch" number except for the untrained model which had no "epoch".

    This is a pragmatic approach to short-term log back-compatibility. It is called on
    all logged events if fix_missing_epoch is True. It can be removed in the future
    if/when we no longer need to read logs from before #1046.
    """
    if "generation" in event:
        assert "epoch" not in event
    elif "epoch" in event:
        warnings.warn(
            'renumbering old "epoch": <n> in event to "generation": <n+1>. '
            "This behaviour will be removed soon",
            DeprecationWarning,
        )
        event["generation"] = event.pop("epoch") + 1
    elif event["event"] in ["rollout_end", "expr_value_info"]:
        warnings.warn(
            'Adding "generation" to epoch-less rollout_end or expr_value_info event. '
            "This behaviour will be removed soon",
            DeprecationWarning,
        )
        event["generation"] = 0
    return event
