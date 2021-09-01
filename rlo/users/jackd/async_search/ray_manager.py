"""Provides ray analogue to `multiprocessing.Manager` class."""
import abc
from collections import deque
import time
from typing import Generic, TypeVar

import ray
from ray.experimental.queue import Empty, Full, Queue

V = TypeVar("V")


class Value(Generic[V]):
    @abc.abstractproperty
    def value(self) -> V:
        raise NotImplementedError("Abstract method")

    @value.setter
    @abc.abstractmethod
    def value(self, value: V):
        raise NotImplementedError("Abstract method")


class _Value(Value):
    """
    Ray-compatible managed `Value` implementation.

    Get/settable `value` attribute.

    Instances should be constructed via `Manager.Value`.
    """

    def __init__(self, actor, uid: int):
        self._actor = actor
        self._uid = uid

    @property
    def value(self) -> V:
        return ray.get(self._actor.value_get.remote(self._uid))

    @value.setter
    def value(self, value: V):
        ray.get(self._actor.value_set.remote(self._uid, value))


class _Queue(Queue):
    """
    Ray-compatible managed `Queue`.

    While this extends from ray.experimental.queue.Queue, this is only for
    `isinstance` checks - there should be no shared implementations.

    Instances should be constructed via `Manager.Queue`.
    """

    # Queue superclass is used for interface / `isinstance` checks only.
    def __init__(self, actor, maxsize: int, uid: int):
        self._actor = actor
        self._uid = uid
        self.maxsize = maxsize

    def __len__(self):
        return self.size()

    def size(self) -> int:
        return ray.get(self._actor.queue_qsize.remote(self._uid))

    def qsize(self) -> int:
        return self.size()

    def empty(self) -> bool:
        return ray.get(self._actor.queue_empty.remote(self._uid))

    def full(self):
        return ray.get(self._actor.queue_full.remote(self._uid))

    def put(self, item, block=True, timeout=None):
        """
        Adds an item to the queue.

        Uses polling if block=True, so there is no guarantee of order if
        multiple producers put to the same full queue.

        Raises:
            Full if the queue is full and blocking is False.
        """
        if self.maxsize <= 0:
            self._actor.queue_put.remote(self._uid, item)
        elif not block:
            if not ray.get(self._actor.queue_put.remote(self._uid, item)):
                raise Full
        elif timeout is None:
            # Polling
            # Use a not_full condition variable or promise?
            while not ray.get(self._actor.queue_put.remote(self._uid, item)):
                # Consider adding time.sleep here
                pass
        elif timeout < 0:
            raise ValueError("'timeout' must be a non-negative number")
        else:
            endtime = time.time() + timeout
            # Polling
            # Use a condition variable or switch to promise?
            success = False
            while not success and time.time() < endtime:
                success = ray.get(self._actor.queue_put.remote(self._uid, item))
            if not success:
                raise Full

    def get(self, block=True, timeout=None):
        """Gets an item from the queue.

        Uses polling if block=True, so there is no guarantee of order if
        multiple consumers get from the same empty queue.

        Returns:
            The next item in the queue.

        Raises:
            Empty if the queue is empty and blocking is False.
        """
        if not block:
            success, item = ray.get(self._actor.queue_get.remote(self._uid))
            if not success:
                raise Empty
        elif timeout is None:
            # Polling
            # Use a not_empty condition variable or return a promise?
            success, item = ray.get(self._actor.queue_get.remote(self._uid))
            while not success:
                # Consider adding time.sleep here
                success, item = ray.get(self._actor.queue_get.remote(self._uid))
        elif timeout < 0:
            raise ValueError("'timeout' must be a non-negative number")
        else:
            endtime = time.time() + timeout
            # Polling
            # Use a not_full condition variable or return a promise?
            success = False
            while not success and time.time() < endtime:
                success, item = ray.get(self._actor.queue_get.remote(self._uid))
            if not success:
                raise Empty
        return item

    def put_nowait(self, item):
        """Equivalent to put(item, block=False).

        Raises:
            Full if the queue is full.
        """
        return self.put(item, block=False)

    def get_nowait(self):
        """Equivalent to get(item, block=False).

        Raises:
            Empty if the queue is empty.
        """
        return self.get(block=False)


class _QueueWrapper:
    """Un-ray-wrapped `ray.experimental.queue._QueueActor`."""

    def __init__(self, maxsize):
        self.maxsize = maxsize
        self._init(maxsize)

    def qsize(self):
        return self._qsize()

    def empty(self):
        return not self._qsize()

    def full(self):
        return 0 < self.maxsize <= self._qsize()

    def put(self, item):
        if self.maxsize > 0 and self._qsize() >= self.maxsize:
            return False
        self._put(item)
        return True

    def get(self):
        if not self._qsize():
            return False, None
        return True, self._get()

    # Override these for different queue implementations
    def _init(self, maxsize):
        del maxsize
        self.queue = deque()

    def _qsize(self):
        return len(self.queue)

    def _put(self, item):
        self.queue.append(item)

    def _get(self):
        return self.queue.popleft()


@ray.remote
class _ManagerActor:
    def __init__(self):
        self._entries = {}
        self._entry_count = 0

    def _uid(self):
        out = self._entry_count
        self._entry_count += 1
        return out

    def Queue(self, maxsize: int = 0) -> int:
        uid = self._uid()
        self._entries[uid] = self._queue(maxsize)
        return uid

    def Value(self, value: V) -> int:
        uid = self._uid()
        self._entries[uid] = value
        return uid

    def value_get(self, uid: int):
        return self._entries[uid]

    def value_set(self, uid: int, value):
        self._entries[uid] = value

    def queue_qsize(self, uid: int) -> int:
        return self._entries[uid].qsize()

    def queue_empty(self, uid: int) -> bool:
        return self._entries[uid].empty()

    def queue_full(self, uid: int) -> bool:
        return self._entries[uid].full()

    def queue_put(self, uid: int, item):
        return self._entries[uid].put(item)

    def queue_get(self, uid: int):
        return self._entries[uid].get()

    # override for custom implementations
    def _queue(self, maxsize: int):  # pylint: disable=no-self-use
        return _QueueWrapper(maxsize)


class Manager:
    """
    Class for managing multiple queues / values on a single ray process.

    This is based on `multiprocessing.Manager`. Currently only `Queue` and `Value`
    supported.
    """

    def __init__(self):
        self._actor = _ManagerActor.remote()  # pylint: disable=no-member

    def Queue(self, maxsize: int = 0) -> Queue:
        return _Queue(self._actor, maxsize, ray.get(self._actor.Queue.remote(maxsize)))

    def Value(self, value: V) -> Value[V]:
        return _Value(self._actor, ray.get(self._actor.Value.remote(value)))
