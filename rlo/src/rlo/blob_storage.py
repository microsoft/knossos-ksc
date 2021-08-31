from typing import Dict, Any
import bz2
import itertools
import json
import logging
import subprocess
import sys
from tqdm import tqdm

from azure.storage.blob import BlockBlobService

from rlo.analytics import events_filename
from rlo.reporting.utils import azure_subscription_context

storageclientlogger = logging.getLogger("azure.storage.common.storageclient")


def progress_func(t):
    def body(current, total):
        t.total = total
        t.update(current)
        t.refresh()

    return body


def get_events_json(block_blob_service, container_name, experiment, i, verbosity):
    events_file_name = events_filename(verbosity, with_bz2=False)
    blob_name = f"{experiment}/{i}/{events_file_name}"
    if block_blob_service.exists(container_name, blob_name):
        with tqdm(desc=f"Downloading {blob_name}", unit="bytes", unit_scale=True) as t:
            blob = block_blob_service.get_blob_to_text(
                container_name, blob_name, progress_callback=progress_func(t)
            )
        return blob.content
    bz2_name = blob_name + ".bz2"
    if block_blob_service.exists(container_name, bz2_name):
        with tqdm(desc=f"Downloading {bz2_name}", unit="bytes", unit_scale=True) as t:
            blob = block_blob_service.get_blob_to_bytes(
                container_name, bz2_name, progress_callback=progress_func(t)
            )
        return bz2.decompress(blob.content)
    print("Didn't find %s" % blob_name, file=sys.stderr)


def _get_block_blob_service(subscription_name, account_name):
    print(
        f"Retrieving storage account keys for storage account {account_name} in subscription {subscription_name}",
        file=sys.stderr,
    )
    with azure_subscription_context(subscription_name):
        account_key = json.loads(
            subprocess.check_output(
                f"az storage account keys list -n {account_name}", shell=True,
            )
        )[0]["value"]

    return BlockBlobService(account_name=account_name, account_key=account_key)


# Cache for _get_block_blob_service
_services: Dict[tuple, Any] = {}


def get_block_blob_service(subscription_name, account_name):
    global _services
    key = (subscription_name, account_name)
    default_value = (
        None
        if key in _services
        else _get_block_blob_service(subscription_name, account_name)
    )
    return _services.setdefault(key, default_value)


def find_blob_root(subscription_name, account_name, container_name, blob_prefix):
    service = get_block_blob_service(subscription_name, account_name)
    all_configs = [
        blob.name
        for blob in service.list_blobs(container_name, blob_prefix)
        if blob.name.endswith("config.json")
    ]
    config_json_path = min(all_configs, key=len)
    blob_root = "/".join(config_json_path.split("/")[:-1])
    print(
        f"Found {config_json_path} in {account_name} / {container_name}",
        file=sys.stderr,
    )
    return blob_root


def get_config(block_blob_service, container_name, blob_root):
    return json.loads(
        block_blob_service.get_blob_to_text(
            container_name, blob_root + "/config.json"
        ).content
    )


def get_events_jsons(block_blob_service, container_name, blob_root, verbosity=0):
    def all_events_jsons():
        for ray_events_dir in ["head", "workers"]:
            ray_events = get_events_json(
                block_blob_service, container_name, blob_root, ray_events_dir, verbosity
            )
            if ray_events is not None:
                yield ray_events

        yield from itertools.takewhile(
            lambda x: x is not None,
            (
                get_events_json(
                    block_blob_service, container_name, blob_root, i, verbosity
                )
                for i in itertools.count(0)
            ),
        )

    return all_events_jsons()
