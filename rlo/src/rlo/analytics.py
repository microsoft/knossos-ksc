# mypy: ignore-errors
"""
This module contains functionality related to logging information about 'events' during RL runs. An event
can be anything from an iteration of search to an epoch of training a neural network, the idea being that logs
will contain anything that might be needed to inspect and visualise what happened during a run.

The 'events' are logged using the standard python logger (`logging.getLogger(__name__)`) throughout the code
by calling the `event()` function defined in this file. The str describing the event type is logged as the
message of the log record, and any additional data is stored using the `fields` attribute of a log record.

When this data is saved for future retrieval, the logs are stored by:
 1) converting each log record to a dictionary (see EventDict). In this dict there will be an entry corresponding
    to the 'event' key, which describes the event type. The remaining entries correspond to what was passed
    to the `fields` attribute of the log-record.
 2) serialising this dictionary to JSON
 3) compressing with bz2

The events saved can have different 'verbosity' levels. The saved event logs will be saved to different files
with each file corresponding to a different verbosity limit, the idea being that if only the logs with some
low verbosity are required, only those logs will have to be downloaded/processed.
"""
import bz2
import contextlib
import json
import logging
import os
import sys
from collections import namedtuple
from typing import Any, Container, Dict, List, Tuple

import numpy as np

from rlo.expression import Expression

# Verbosity levels. These work the other way around to python logging levels, so we map to the lowest-importance
# levels there (INFO and lower) - everything below WARNING is by default ignored by the root python Logger.
MIN_VERBOSITY = 0  # "Low" verbosity = few messages = the most important
MAX_VERBOSITY = logging.INFO - 1  # Highest verbosity = most messages = all the drivel
DEFAULT_VERBOSITY_LIMIT = 1  # Highest verbosity level that is captured (by default)


# An event is a dictionary with str keys with at least least one entry for key 'event' specifying the event type
# This will usually be serialised to json when logs are saved
EventDict = Dict[str, Any]
# (TODO: this should be replaced with Literal[0, 1] once we upgrade to Python 3.8)
VerbosityType = int


class LogJSONEncoder(json.JSONEncoder):
    """Encode all numpy varieties of float as floats; and Expressions as their strings."""

    # pylint: disable=method-hidden
    def default(self, o):
        if isinstance(o, np.floating):
            return float(o)
        if isinstance(o, Expression):
            return str(o)
        return super().default(o)


class RecordFormatter(logging.Formatter):
    def format(self, record: logging.LogRecord) -> str:
        # "fields" added to the logrecord from the "extra" dict in log()/event() below
        return "{} {}".format(record.msg, record.fields)


class JsonFormatter(logging.Formatter):
    def format(self, record: logging.LogRecord) -> str:
        return json.dumps({"event": record.msg, **record.fields}, cls=LogJSONEncoder)


# The topmost logger defines a "topic", we have just one for all analytics, that never changes.
_py_logger = logging.getLogger(__name__)
_py_logger.setLevel(logging.INFO - MAX_VERBOSITY)
_py_logger.propagate = (
    False  # Prevents default logging to stderr (in only some environments?!)
)
assert _py_logger.isEnabledFor(logging.INFO - MAX_VERBOSITY)


def _add_handler(
    handler: logging.Handler, formatter: logging.Formatter, verbosity: VerbosityType
) -> None:
    """Add a logging handler to the global logger. Set the formatter and level of that handler."""
    handler.setFormatter(formatter)
    handler.setLevel(logging.INFO - verbosity)
    _py_logger.addHandler(handler)


_add_handler(logging.StreamHandler(sys.stdout), RecordFormatter(), MIN_VERBOSITY)


@contextlib.contextmanager
def log_events_to_file(
    fname: str, verbosity_limit: VerbosityType = MIN_VERBOSITY, overwrite: bool = False
):
    """
    Log events to a bz2-compressed file.
    Args:
      fname: path to write logs to.
      verbosity_limit: all events with verbosity<=verbosity_limit will be logged.
      overwrite: if True and fname exists, replace it. If False and fname exists, append to the file.
    """
    directory, _ = os.path.split(fname)
    if not os.path.exists(directory):
        os.makedirs(directory)
    with bz2.open(fname, "wt" if overwrite else "at", encoding="utf-8") as f:
        handler = logging.StreamHandler(stream=f)
        _add_handler(handler, JsonFormatter(), verbosity_limit)
        yield
        handler.flush()
        _py_logger.removeHandler(handler)


def events_filename(level: VerbosityType, with_bz2: bool = True) -> str:
    """
    Returns an event filename string of the form events{_level}.json{.bz2}.

    Args:
        level: If 0, returns string events.json. Otherwise, appends the level, e.g. events_5.json.
        with_bz2: Whether to append '.bz2' to the file extension.
    """
    suffix = "" if level == 0 else f"_{level}"
    file_extension_suffix = ".bz2" if with_bz2 else ""
    return f"events{suffix}.json{file_extension_suffix}"


def log_events_to_files(
    path_prefix: str,
    verbosity_limit: VerbosityType = DEFAULT_VERBOSITY_LIMIT,
    overwrite: bool = False,
) -> contextlib.ExitStack:
    """
    Starts logging all events up to and including verbosity_limit into files events.json, events_1.json, ..., events_<verbosity_limit>.json
    where events_X.json will have all events up to and including verbosity X - i.e. one file for each level of
    verbosity up to and including the specified limit.

    Returns:
        contextlib.ExitStack: a context manager that ends logging when it is exited.
    """
    assert verbosity_limit >= 0
    with contextlib.ExitStack() as stack:
        # pylint: disable=no-member # Resolves ExitStack as AbstractContextManager w/out pop_all or enter_context,
        # unless we downgrade packages to pylint<2.4 astroid<2.3 :(
        for i in range(verbosity_limit + 1):
            path = path_prefix + events_filename(level=i)
            stack.enter_context(
                log_events_to_file(path, verbosity_limit=i, overwrite=overwrite)
            )
        # The current 'stack' will exit all the ContextManagers we've just created
        # when we leave this with block. The following call prevents that by transferring
        # each of the individual context managers to a new stack which we return.
        return stack.pop_all()


class LogEventsToList(logging.Handler):
    def __init__(self, verbosity_limit: VerbosityType = MIN_VERBOSITY) -> None:
        super().__init__()
        self._verbosity_limit = verbosity_limit
        self._log_items: List[Tuple[EventDict, VerbosityType]] = []

    def __enter__(self) -> "LogEventsToList":
        _add_handler(self, None, self._verbosity_limit)
        return self

    def __exit__(self, exc_type, exc_value, trace) -> None:
        _py_logger.removeHandler(self)

    def emit(self, record: logging.LogRecord) -> None:
        self._log_items.append(
            ({"event": record.msg, **record.fields}, logging.INFO - record.levelno)
        )

    @property
    def log_items(self) -> List[EventDict]:
        return [event for event, _ in self._log_items]

    @property
    def log_items_with_verbosity(self) -> List[Tuple[EventDict, VerbosityType]]:
        return self._log_items


class Scope:
    """A context manager that adds extra fields to every event issued within its scope."""

    def __init__(self, parent=None, /, **extra_fields):
        """
        Parent may be a single Scope passed by position, or absent to use the current scope as parent.

        Passing parent=xyz will add that field to the log messages, just as any other field=value in extra_fields.
        """
        parent = parent or get_current_scope()
        self.fields = {**parent.fields, **extra_fields}

    def __enter__(self):
        _scopes.append(self)
        return self

    def __exit__(self, _exc_type, _exc_value, _trace):
        assert _scopes[-1] is self
        _scopes.pop()
        return False  # Do not suppress any exception being raised


# This records the Scopes that have been entered/exited as nested context managers.
# The final element is that currently in use.
_scopes = [
    Scope(
        # This just creates something with an empty 'fields'
        namedtuple("RootScope", ["fields"])({})
    )
]


def get_current_scope():
    """
    Return the Scope currently in use.
    
    This can be used as a context manager or a parent for a new Scope.
    """
    return _scopes[-1]


def event(event_type: str, verbosity: VerbosityType = 0, **kwargs) -> None:
    """Log an event.

    Args:
        event_type: Type of event e.g. 'search', 'distillation_epoch'.
        verbosity: The logging level to log this event at.
        kwargs: Any number of named fields that will be recorded as part of the event.
    """
    _py_logger.log(
        logging.INFO - verbosity,
        event_type,
        # pylint: disable=no-member  # pylint 2.4.4 doesn't understand the constructor
        # because of the position-only parameter.
        extra={"fields": {**get_current_scope().fields, **kwargs}},
    )


class EventTypeFilter(logging.Filter):
    """Class for filtering logs based on event types."""

    def __init__(self, event_types: Container[str]):
        self.event_types = event_types

    def filter(self, rec: logging.LogRecord) -> bool:
        return rec.msg in self.event_types


class FilteredLogger:
    """Context manager to filter logs."""

    def __init__(self, filter: logging.Filter):
        self.filter = filter

    def __enter__(self):
        global _py_logger
        _py_logger.addFilter(self.filter)

    def __exit__(self, exc_type, exc_value, trace):
        global _py_logger
        _py_logger.removeFilter(self.filter)
        return False
