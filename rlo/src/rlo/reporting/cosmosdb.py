import json
import subprocess
from typing import Dict, Optional, Tuple

from azure.cosmos import CosmosClient
import numpy as np

from . import azureml
from .utils import azure_subscription_context, RemoteRunInfo

SUBSCRIPTION_NAME = "knossos"
COSMOS_DB_RESOURCE_GROUP = "knossosrlodbrg"
COSMOS_DB_ACCOUNT_NAME = "knossosrlosrvlessdb"
COSMOS_CONNECTION_URL = f"https://{COSMOS_DB_ACCOUNT_NAME}.documents.azure.com:443/"


def _get_cosmos_db_read_only_key(subscription_name, account_name, resource_group):
    print(
        f"Retrieving access keys for Cosmos DB {account_name} in resource group {resource_group}",
    )
    with azure_subscription_context(subscription_name):
        return json.loads(
            subprocess.check_output(
                f"az cosmosdb keys list -n {account_name} -g {resource_group} --type read-only-keys",
                shell=True,
            )
        )["primaryReadonlyMasterKey"]


def get_cosmos_container(container_name, read_only=True, allow_interactive=True):
    if read_only:
        cosmos_db_key = _get_cosmos_db_read_only_key(
            SUBSCRIPTION_NAME, COSMOS_DB_ACCOUNT_NAME, COSMOS_DB_RESOURCE_GROUP
        )
    else:
        # This may raise if authentication fails
        cosmos_db_key = azureml.get_secret(
            "cosmosdbkey", allow_interactive=allow_interactive
        )
    client = CosmosClient(COSMOS_CONNECTION_URL, cosmos_db_key)
    db = client.get_database_client("knossosrlodb")
    return db.get_container_client(container_name)


def _sanitize_float(val):
    if np.isnan(val):
        return "nan"
    if np.isinf(val):
        return "inf" if val > 0 else "-inf"
    return val


def _encode_config(config):
    """ Cleanup nan / inf from config ahead of JSON serialization so that Cosmos DB can handle it.
    """
    return {
        # Only handles top-level nan / inf
        k: _sanitize_float(v) if isinstance(v, float) else v
        for k, v in config.items()
    }


def _decode_config(config):
    return {
        k: float(v) if v in ["nan", "inf", "-inf"] else v for k, v in config.items()
    }


def upload_run_to_db(
    config: Dict, info: Optional[RemoteRunInfo] = None, allow_interactive: bool = True
):
    try:
        container = get_cosmos_container(
            "runs", read_only=False, allow_interactive=allow_interactive
        )
    except Exception as e:
        # Print out the error and give up upload
        print(f"Cannot upload config to Cosmos DB. Got exception {e}")
        return
    if not info:
        info = azureml.get_current_run_info()
    container.create_item(
        {
            "id": config["run_id"],
            "config": _encode_config(config),
            "remote_run_info": info._asdict(),
        }
    )


class RunInfoNotFound(Exception):
    pass


def get_run_from_db(run_id: str) -> Tuple[Dict, RemoteRunInfo]:
    container = get_cosmos_container("runs")
    query = container.query_items(
        f'SELECT * FROM c WHERE c.id = "{run_id}"', enable_cross_partition_query=True
    )
    try:
        doc = next(query)
        config = _decode_config(doc["config"])
        info = RemoteRunInfo(**doc["remote_run_info"])
        return config, info
    except StopIteration as e:
        raise RunInfoNotFound(
            f"""Could not find run {run_id} in the Cosmos DB. It could be
 - a run that ran in a different workspace (e.g., resrchvc),
 - a run that predates the DB, or
 - an azure batch run.
"""
        ) from e


def get_remote_run_info_from_db(run_id: str) -> RemoteRunInfo:
    _, info = get_run_from_db(run_id)
    return info


def check_and_upload_config_to_db(config: Dict, info: RemoteRunInfo):
    run_id = config["run_id"]
    try:
        remote_config, remote_info = get_run_from_db(run_id)
    except RunInfoNotFound:
        print(f"Could not find run {run_id} in Cosmos DB. Uploading...")
        upload_run_to_db(config, info)
        return
    assert remote_config == config
    assert remote_info == info
