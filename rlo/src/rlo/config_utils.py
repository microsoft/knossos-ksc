"""
Utility functions for operating with global config.

"""

import multiprocessing
import os
import sys
import traceback
from typing import Any, Callable, Dict, Iterable, Optional, Tuple, Sequence


def kwargs_from_config(
    config: Dict[str, Any],
    required_keys: Iterable[str],
    optional_keys: Iterable[str],
    renames: Optional[Iterable[Tuple[str, str]]] = None,
) -> Dict[str, Any]:
    """
    Extract from a dictionary a dictionary with only those keys specified as required_keys or optional_keys.
    Keys that appear in 'renames' are treated as optional.
    """
    kwargs = {k: config[k] for k in required_keys}
    kwargs.update({k: config[k] for k in optional_keys if k in config})

    if renames is not None:
        for old_name, new_name in renames:
            if old_name in config:
                kwargs[new_name] = config[old_name]
    return kwargs


def unify_configs(configs, keys, allow_renames={}) -> Dict[str, Any]:
    """ Returns a single config dict from a list of configs making
        sure that the values match for the key in keys

    Args:
        configs: list of configs to be unified
        keys: list of keys to include in the unified config
        allow_renames: (optional) a mapping from a new key to an old key
            name that can be used instead. We don't support multiple old
            keys being renamed to a new key because it is unlikely.
    """
    assert all(new_key in keys for new_key in allow_renames)
    common_config = {}
    for config in configs:
        for key in keys:
            if key in config:
                current_value = config[key]
            elif key in allow_renames:
                old_key = allow_renames[key]
                current_value = config[old_key]
            else:
                raise ValueError("Could not find {} in {}".format(key, config))
            if key not in common_config:
                common_config[key] = current_value
                continue
            if common_config[key] != current_value:
                raise ValueError(
                    "Expected {}"
                    " for key {}, but got {}"
                    " in {}".format(common_config[key], key, config[key], config)
                )
    return common_config


def _multiprocess_wrapper(device_id, func, args, kwargs):
    # args/kwargs are each single arguments being a list and a dict respectively
    os.environ["CUDA_VISIBLE_DEVICES"] = str(device_id)
    try:
        res = func(*args, **kwargs)  # unpack into multiple arguments
    except:
        # allow a repetition to fail but log the failure
        print("Task {} failed with the following traceback:".format(func))
        traceback.print_exc()
        res = None
    sys.stdout.flush()
    return res


def multiprocess(
    num_workers: int, func: Callable, per_task_args: Sequence[Any], *args, **kwargs
):
    """
    Runs tasks over multiprocessing.Pool. If num_workers is greater than one,
    CUDA_VISIBLE_DEVICES will be set so that each worker gets a different GPU device.

    Args:
      num_workers: the maximum number of parallel processes between which to divide the tasks
      func: a function to be invoked for each task
      per_task_args: an Iterable, with one element per parallel task to be performed,
          being the first arg to <func> for that task
      *args, **kwargs: additional arguments
    """
    if num_workers == 1:
        # Avoid unnecessary multiprocessing as this helps with profiling
        return [func(task_arg, *args, **kwargs) for task_arg in per_task_args]

    chunksize = (len(per_task_args) + num_workers - 1) // num_workers
    with multiprocessing.Pool(processes=num_workers) as pool:
        return pool.starmap(
            _multiprocess_wrapper,
            [
                (i // chunksize, func, (task_arg,) + args, kwargs)
                for i, task_arg in enumerate(per_task_args)
            ],
            chunksize=chunksize,
        )


def config_for_repetition(config: Dict[str, Any], repetition: int) -> Dict[str, Any]:
    if config.get("repetition") is not None:
        # We have no idea what path(s) would/n't be correct here.
        raise ValueError(
            "Cannot override repetition {} from config with {}".format(
                config["repetition"], repetition
            )
        )
    return {
        **config,
        "repetition": repetition,
        "result_save_path": os.path.join(config["result_save_path"], str(repetition)),
    }
