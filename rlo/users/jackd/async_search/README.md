# Asynchronous Parallel Search Prototypes

This project demonstrates prototype implementations for using async / multiprocessing / ray for more efficient parallel search.

## Quick Start

```bash
pip install seaborn absl-py ray tqdm
# from knossos repo directory
export PYTHONPATH=$PWD/src
cd users/jackd/async_search
python main.py
```

### Sample results

The following were performed on a 12-core Asus laptop with tensorflow 2.2 and NVidia GTX-1050-Ti GPU.

```bash
>> python main.py --continuous
Finsihed continuous async
Total search time: 20.63 s
Total model time : 18.96 s
Fraction in model: 91.93 %
Batch info
  num_batches:  44
  num elements: 16003
  size mean:    363.70
  size std:     268.26
>> python main.py --multi_server
Finsihed multi_server_ray(managed_queues=False, num_procs=2, num_servers=2, total_gpu_frac=0.6)
Total search time: 37.13 s
>> python main.py  # results.png below
```

![python main.py results](./results.png)

## Overview

We identify two potential speed improvements over the synchronous search implementations currently in knossos:

1. poor resource utilization due to alternating CPU and GPU operations; and
2. difficulty efficiently batching due to variable number of neighbors of each node.

The base implementations in this prototype tackle both these problems by running several searches asynchronously and possibly simultaneously. This allows the GPU process(es) to run inference on some search requests while CPU processes can be kept busy running other, independent search coroutines.

`continuous_astar` is the exception - it is an implementation designed to be asynchronous from the start, so receives benefits even when only a single search is being performed.

The general idea is to

1. create one or more `ConnectedFunction`s and a server connection;
2. create an `asyncio.Task` for each `ConnectedFunction` which performs one or more searches.

At this point, the following must be performed either simultaneously across multiple processes or interleaved on a single process.

* run tasks;
* periodically `flush` the `ConnectedFunction`s; and
* serve the server connection.

## Experiments

`main.py` provides three experiments.

### Default

Default running of `main.py` runs various configurations of 3 asynchronous implementations that execute search algorithms very similar to those used in knossos. Slight simplifications have been applied, though these should not affect performance significantly. Our implementations include those using

* a single-process;
* multiprocessing; and
* ray.

These experiments all serve the server connection from the main process.

### --multi_server

Using the `--multi_server` flag runs the same search algorithms as default but with multiple servers using ray. Default parameter values are designed to be run locally with a single GPU machine, though this is not the intended environment for multiple servers.

### --continuous

Using the `--continuous` flag runs a modified astar search, where queries are made continuously and the server serves as fast as possible. This means search will vary depending on how fast model inferences are served.

## Class Structure

### Queue

In order to accomodate ray and multiprocessing, we consider FIFO `QueueLike` objects. Implementations include

* [queue.Queue](https://docs.python.org/3/library/queue.html), for single-process applications;
* [multiprocessing.Queue](https://docs.python.org/3.6/library/multiprocessing.html#multiprocessing.Queue) (created via `multiprocessing.Manager.Queue`) for multiprocessing applications; and
* [ray.experimental.queue.Queue](https://github.com/ray-project/ray/blob/master/python/ray/experimental/queue.py) or [ray_manager.Manager.Queue](ray_manager.py) for ray applications.

### Connection

`Connection`s are generalizations of `multiprocessing.connection.Connection` and are based on `QueueLike`s. Conceptually, connections allow objects to make and serve requests. In this project we typically have client connections and server connections, where search functions use client connections to request inferences via a `ConnectedFunction`, and a server function manages batching and GPU inference to receive requests and snend responses via the server connection.

See [example_connection](example_connection.py) for a simplified example.

#### Pipes

We refer to two connected connections as a pipe, where items sent with the `send` method on one can be recevied with `recv` on the other. These are constructed with

* `pipe`: single two-way pipe; and
* `forked_pipe`: provide multiple connections feeding into a single request queue and manages responses (assuming they are made in the same order as requests are made).

### ConnectedFunction

Connected functions are asynchronous function interfaces to client connections. They come in two varieties:

* `FifoConnectedFunction`: assumes the other end of the connection returns results in the same order as the requests are made; and
* `UnorderedConnectedFunction`: makes no assumptions about the order of the returned request.

Depending on the number of servers and clients you wish to use, these are generally created with `get_connected_functions`, which will return a sequence of functions that can be used independently and a server connection. If you wish to use multiple servers to serve the server connection, you should create a copy of this server connection for each server.

## Serving Connections

`servers.py` provides methods for serving server connections that feed results to `ConnectedFunction`s.

### Batching

Initial implementations served single-element requests. This resulted in a high degree of traffic (across processes in the multiprocessing / ray case) and led to traffic bottlenecks. The current implementation serves sequence requests - i.e. `ConnectedFunction`s accept sequences and return a single future with an eventual result associated with all the inputs. The server has tasks:

1. Receive sequence requests from the connection and batch them.
2. Compute the function on that batch.
3. Take the result and split it up according to the received sizes.

For example, if a server wants to use a batch size of `8` and receives requests of size `(5, 4, 6, 3)`, it should:

1. Take the top two requests.
2. Compute function outputs on the first 8 elements.
3. Send a response to the first request.
4. Take the third and forth request.
5. Compute function outputs on second 8 elements.
6. Respond to the second and third request.
7. Compute function outputs on the final 2 elements.
8. Respond to the forth request.

To make things more complicated, the batch size may not be fixed (e.g. when using `SparsePipeline`s).

`batchers.py` provides 3 classes for managing this process.

* `Batcher` (`ABC`): provides a mechanism to split a stream of sequences into a different stream of sequences. Curretly the only implementation is `BatchSizeBatcher`, but batching according to maxium number of nodes in a batch would be relatively straight forward.
* `Unbatcher`(`ABC`): provides the inverse of `Batcher`. It records request sizes as they come in and yields results as batched outputs are supplied. Currently the only implementation is `NumpyUnbatcher` which assumes function outputs are a single numpy array with a one-to-one correspondence with request elements.
* `BatchManager`: coordinates a `Batcher` and `Unbatcher`.
