import abc
import asyncio
from typing import (
    Any,
    Callable,
    Dict,
    Generic,
    Mapping,
    Optional,
    Sequence,
    Tuple,
    TypeVar,
    Union,
)
from collections import deque

from ray.experimental.queue import Queue as _ray_Queue
from multiprocessing import Queue as _mp_Queue
from queue import Queue as _Queue

QueueLike = Union[_ray_Queue, _mp_Queue, _Queue]
QueueFactory = Callable[[int], QueueLike]

I = TypeVar("I")  # inputs, arg to send
O = TypeVar("O")  # outputs, from recv
K = TypeVar("K")  # key


def identity(x):
    return x


class ClosedConnection(Exception):
    pass


class Connection(Generic[I, O]):
    """
    Ray-compatible connection object similar to those from `multiprocessing.pipe`.

    Allows two-way communication between ray processes.
    """

    def __init__(self, inbound: QueueLike, outbound: QueueLike):
        self._inbound = inbound
        self._outbound = outbound

    def num_waiting(self) -> int:
        return self._inbound.qsize()

    def poll(self) -> bool:
        """Whether or not there is any data waiting to be `recv`ed."""
        return self.num_waiting() > 0  # don't use empty - bugged in ray 0.8.7

    def send(self, item: I, block: bool = True, timeout: Optional[float] = None):
        return self._outbound.put(item, block=block, timeout=timeout)

    def recv(self, block: bool = True, timeout: Optional[float] = None) -> O:
        return self._inbound.get(block=block, timeout=timeout)

    def copy(self):
        """Create a copy backed by the same `Queue`s."""
        return self


class ForkedConnection(Connection[I, O]):
    """
    Connection with a single inbound and multiple outbound queues.

    Elements pushed to `inbound` are expected to be of the form `(key, inp)`, where
    `key` is the key associated with an entry of `outbounds` (constructor), or
    previously used as an argument to `add_outbound`. For example, this could be the
    `outbound` queue of a `KeyedConnection`.

    Users of this connection are assumed to `send` results associated with `inp` back in
    the same order as they are received using `recv`. `recv` only returns the `inp` from
    the inbound queue element.
    """

    def __init__(self, inbound: QueueLike, outbounds: Mapping[Any, QueueLike]):
        self._inbound = inbound
        self._outbounds = outbounds
        self._keys = deque()

    def copy(self) -> "ForkedConnection[I, O]":
        # uses a different `_keys` deque.
        return ForkedConnection(self._inbound, self._outbounds)

    def send(self, item: I, block: bool = True, timeout: Optional[float] = None):
        return self._outbounds[self._keys.popleft()].put(
            item, block=block, timeout=timeout
        )

    def recv(self, block: bool = True, timeout: Optional[float] = None) -> O:
        key, value = self._inbound.get(block=block, timeout=timeout)
        self._keys.append(key)
        return value


class DelegatingConnection(Connection[I, O]):
    """
    Connection that delegates all methods to `conn`.

    While not abstract, this is intended to be used as a base class.
    """

    def __init__(self, conn: Connection):
        self._conn = conn

    def num_waiting(self) -> int:
        return self._conn.num_waiting()

    def poll(self) -> bool:
        return self._conn.poll()

    def send(self, item: I, block: bool = True, timeout: Optional[float] = None):
        return self._conn.send(item, block=block, timeout=timeout)

    def recv(self, block: bool = True, timeout: Optional[float] = None) -> O:
        return self._conn.recv(block=block, timeout=timeout)


class KeyedConnection(Generic[K, I, O], DelegatingConnection[Tuple[K, I], O]):
    """
    Connection with key associated with each entry.

    `self.send(item, **kwargs)` is equivalent to `conn.send((key, item), **kwargs)`.

    These can be used in conjunction with `ForkedConnection` to allow multiple
    connections be paired with one other connection. See `forked_pipe` for construction
    of such connections.

    """

    def __init__(self, key: K, conn: Connection[I, O]):
        self._key = key
        DelegatingConnection.__init__(self, conn)

    @property
    def key(self) -> K:
        return self._key

    def send(self, item: I, block: bool = True, timeout: Optional[float] = None):
        return super().send((self._key, item), block=block, timeout=timeout)


def forked_pipe(
    queue_factory: QueueFactory, num_branches: int, maxsize: int = 0
) -> Tuple[ForkedConnection, Sequence[KeyedConnection]]:
    """
    Provides a set of connections that can be used as pipes.

    If `forked, keyed = forked_pipe(...)`, then each of `[(forked, k) for k in keys]`
    can be used as a pipe.

    Input queues must be empty during construction and any subsequent put / get actions
    on these queues will almost certainly break the returned connections.

    Args:
        root_queue: queue that `forked` receives from and all of `keyed` send to.
        branch_queues: queues that `forked` sends to and `keyed` receive on.

    Returns:
        forked: connection that pairs with `keyed`
        keyed: sequence of connections with the same length as `branch_queues` that each
            pair with `forked`.
    """
    root_queue = queue_factory(maxsize)
    branch_queues = [queue_factory(maxsize) for _ in range(num_branches)]
    outbounds = dict(enumerate(branch_queues))
    forked = ForkedConnection(root_queue, outbounds)
    keyed = tuple(
        KeyedConnection(i, Connection(q, root_queue))
        for i, q in enumerate(branch_queues)
    )
    return forked, keyed


class PassThroughConnection(DelegatingConnection[I, O]):
    """
    Connection that passes metadata for each value up/down unchanged.

    The inbound queue is expected to have elements of the form `(meta, inp)`. The
    outbound queue is populated with elements of the form `(meta, output)`. For each
    `recv` a corresponding `send` is expected, and the order in which the `send`s are
    called is expected to be in the same order as elements are `recv`ed.

    This is helpful when multiple servers are servicing the same inbound queue. If
    `meta` is hashable, it can be used to match elements `recv`ed from `inbound` to
    elements send (via `send`) to `outbound`.
    """

    def __init__(self, conn: Connection[Tuple[Any, I], Tuple[Any, O]]):
        super().__init__(conn=conn)
        self._metas = deque()

    def copy(self):
        # different `_metas` deque
        return PassThroughConnection(self._conn.copy())

    def send(self, item: I, block: bool = True, timeout: Optional[float] = None):
        meta = self._metas.popleft()
        return super().send((meta, item), block=block, timeout=timeout)

    def recv(self, block: bool = True, timeout: Optional[float] = None) -> O:
        meta, value = super().recv(block=block, timeout=timeout)
        self._metas.append(meta)
        return value


def pipe(
    queue_factory: QueueFactory, maxsize: int = 0
) -> Tuple[Connection[I, O], Connection[O, I]]:
    """
    Get a pair of `Connection` objects built from queues.

    Each connection's `send` will be linked to the other's `recv`.
    """
    q0 = queue_factory(maxsize)
    q1 = queue_factory(maxsize)
    return Connection(q0, q1), Connection(q1, q0)


class ConnectedFunction(Generic[I, O]):
    """
    Function which delegates computation to a connection.

    This is useful e.g. when running several tasks each of which make requests which
    are handled by a service that can batch requests.

    The class has two main methods:
        * `__call__`: sends a request over `conn` and returns an `asyncio.Future`.
        * `flush`: completes futures based on responses received on `conn`.

    `run_while` and `run_until_complete` can be used to periodically `flush` results.

    See `FifoConnectedFunction` and `UnorderedConnectedFunction` for implementations.

    Example usage:

    ```python
        import asyncio
    from queue import Queue
    import numpy as np
    from connections import pipe, Connection, ConnectedFunction, FifoConnectedFunction


    async def repeat(fn: ConnectedFunction, x):
        for _ in range(3):
            x = await fn(x)
        return x


    def flush_server(conn: Connection, server_fn):
        # see servers.py for more sophisticated serving functions
        while server_conn.poll():
            server_conn.send(server_fn(conn.recv()))


    client_conn, server_conn = pipe(Queue(), Queue())
    connected_fn = FifoConnectedFunction(client_conn)

    task = asyncio.gather(*(repeat(connected_fn, [i, i + 1]) for i in range(5)))
    loop = task.get_loop()
    while not task.done():
        loop.stop()
        loop.run_forever()
        # process server connection
        flush_server(server_conn, np.square)
        connected_fn.flush()
    print(task.result())
    ```
    """

    def __init__(self, conn: Connection, preprocess_fn: Callable = identity):
        """
        Args:
            conn: Connection over which to send requests / receive results.
            preprocess_fn: function applied to inputs before request is made.
        """
        self._conn = conn
        self._preprocess_fn = preprocess_fn
        self._init()

    def _init(self):
        """Custom initialization called after `__init__`."""

    def __call__(self, inputs: Sequence[I]) -> "asyncio.Future[O]":
        future = asyncio.Future()
        self._conn.send(self._send(self._preprocess_fn(inputs), future))
        return future

    @abc.abstractmethod
    def _send(self, processed_inputs, future: asyncio.Future):
        raise NotImplementedError("Abstract method")

    @abc.abstractmethod
    def _on_recv(self, received):
        raise NotImplementedError("Abstract method")

    def flush(self):
        while self._conn.poll():
            self._on_recv(self._conn.recv())

    def run_while(self, cond: Callable[[], bool]):
        while cond():
            self.flush()

    def run_until_complete(self, task: asyncio.Task):
        loop = task.get_loop()
        while not task.done():
            loop.stop()
            loop.run_forever()
            self.flush()
        return task.result()


class FifoConnectedFunction(ConnectedFunction[I, O]):
    """
    `ConnectedFunction` for connections with consistent `send` / `recv` order.

    This is useful when connected to a single server.

    If order is not guaranteed to be consistent, use `UnorderedConnectedFunction`
    and wrap server connections in a `PassThroughConnection`.
    """

    def _init(self):
        self._futures = deque()

    def _send(self, processed_inputs, future: asyncio.Future):
        self._futures.append(future)
        return processed_inputs

    def _on_recv(self, received: O):
        self._futures.popleft().set_result(received)


class UnorderedConnectedFunction(ConnectedFunction[I, O]):
    """
    `ConnectedFunction` for connections without consistent `send` / `recv` order.

    This is useful when connected to multiple servers which process parallel requests,
    in which case consider wrapping server connections in `PassThroughConnection`.

    If order is guaranteed to be consistent, use `FifoConnectedFunction`.
    """

    def _init(self):
        self._send_count = 0
        self._futures: Dict[int, asyncio.Future] = {}

    def _send(self, processed_inputs, future: asyncio.Future):
        uid = self._send_count
        self._send_count += 1
        self._futures[uid] = future
        return (uid, processed_inputs)

    def _on_recv(self, received):
        uid, result = received
        self._futures.pop(uid).set_result(result)


def get_connected_functions(
    queue_factory: QueueFactory,
    num_fns: int,
    ordered: bool,
    maxsize: int = 0,
    preprocess_fn: Callable = identity,
) -> Tuple[Sequence[ConnectedFunction[I, O]], Connection[O, I]]:
    """
    Get a sequence of connected functions, along with the connection to serve them.

    Args:
        queue_factory: Factory for creating queues, e.g. this may be queue.Queue for
            multithreading, multiprocessing.Manager().Queue for multiprocessing,
            ray.experimental.Queue for ray with separate actors for each queue
            or ray_manager.Manager().Queue for ray with a single actor managing all
            queues.
        num_fns: number of connected functions to return.
        ordered: if True, the connected functions use an optimized form that assume
            the connection is served in order.
        maxsize: maximum size of each underlying queue.
        preprocess_fn: function applied to inputs before passing over connections.

    Returns:
        connected_fns: sequence of `ConnectedFunction`s served by connection
        connection: `Connection` to provide values for the connected functions.
            If `ordered` is `True`, responses are expected to be sent via `send`
            in the same order they were recevied via `recv`.
    """
    if num_fns == 1:
        c0, c1 = pipe(queue_factory, maxsize)
        c1s = (c1,)
        del c1
    else:
        c0, c1s = forked_pipe(queue_factory, num_fns, maxsize)
    if ordered:
        client_factory = FifoConnectedFunction
    else:
        client_factory = UnorderedConnectedFunction
        c0 = PassThroughConnection(c0)
    client_fns = tuple(client_factory(c1, preprocess_fn=preprocess_fn) for c1 in c1s)
    return client_fns, c0
