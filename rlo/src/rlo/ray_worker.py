# fmt: off
import contextlib
import os
import ray
import time
from typing import Callable, Dict, Optional, Tuple
import weakref

from rlo import analytics
from rlo.factory import NoGPUError
from rlo.tf_model import Weights
from rlo import utils
from rlo.local_worker import WorkerWithModel, measure_time
from rlo.worker import WorkerPool

class RayWorkerPool(WorkerPool):
    """ Runs jobs remotely using a variable-size pool of workers. """
    def __init__(self, base_config, remote_timeout=3600, local_task_limit=0, worker_events_dir=None):
        """ base_config should be sufficient to build a new Model.
            worker_events_dir is a directory into which events(_1).json.bz2, containing events from tasks run on remote workers, are saved.
                if None, it's calculated from the base_config.
            Other parameters control ray task execution:
                remote_timeout is the max time for which to run a task remotely; any longer and we assume the worker is dead/AWOL (and rerun task with *2 timeout).
                local_task_limit: if >0, throw an exception after any local task/continuation that took a greater number of seconds.
        """
        super().__init__()
        self._base_config = base_config
        # pylint: disable=no-member # The options member is added by the ray.remote decorator
        cpus,gpus = (0, 1/base_config["workers_per_gpu"]) if base_config.get("force_gpu", False) else (1,0)
        self._worker_class = _RemoteWorker.options(num_cpus=cpus, num_gpus=gpus)
        self._workers = []
        self._next_worker_idx = 0
        # Cache a Ray Object ID (result of ray.put) for each distinct weights object, keyed by id(). Ray doesn't seem to realize
        # that the same object is the same object so this ensures each weights object appears in the object store only once.
        self._weight_id_map = {}
        # Hang onto any Worker objects which threw Exceptions from their constructors - these are probably due to bad machines
        # (CUDA errors etc.). If we allow the Workers to be garbage collected, Ray would be able to try constructing a Worker
        # on the same node again (and again)
        self._unusable_workers = []
        # Tasks executing remotely: from ray ID of result (i.e. a future/promise), to name and no-arg lambda to call when result ready
        self._pending_ids: Dict[ray.ObjectID, Tuple[str, Callable]] = dict()
        # For ObjectIDs representing tasks we are waiting to execute, record the issue time, item and the Worker to kill
        self._timeouts: Dict[ray.ObjectID, Tuple[float, WorkerPool.WorkItem, _RemoteWorker]] = dict()
        self._timeout_lengths: Dict[WorkerPool.WorkItem, float] = weakref.WeakKeyDictionary()
        self._default_timeout = remote_timeout
        self._local_task_limit = local_task_limit
        # Tasks to run in this process, where they can mutate state, when we have time. Data are tuples
        # (name, func, time_acc, ...args to pass to func...) where all beginning with func can be passed to measure_time
        self._local_tasks = utils.UrgencyQueue()
        # self._workers set by factory method
        self._start_time = None # Indicates we are not running
        self._worker_events_dir = worker_events_dir or os.path.join(base_config["result_save_path"], "workers")

    def _dump_logs(self, bz2s):
        assert len(bz2s) == len(self._worker_events_files)
        for (bz2_bytes, file) in zip(bz2s, self._worker_events_files):
            file.write(bz2_bytes)
            file.flush()
        
    def _worker_died(self, wrkr: "_RemoteWorker", item: WorkerPool.WorkItem,
            worker_death_time: float, item_issue_time: float, reason: Optional[str]):
        """ Handle death of <wrkr> during execution of <item>. reason is exception string, or None if we killed it (timeout). """
        # Put task back on queue to retry on another worker; forget the logs from this attempt..
        self._items.put(item.urgency, item)
        self._num_workers -= 1
        analytics.event("worker_died", time=worker_death_time - self._start_time, taskname=item.name,
                        worker=wrkr.desc, # type: ignore
                        reason=reason, time_lost=(worker_death_time - item_issue_time),  
                        **self._worker_counts())
        # Drop the Worker object so we don't send any more tasks to it. It'll be reclaimed, and if the body
        # hosting the worker is still running, the resources will become free and Ray will be able to create
        # a new Worker object on it (from the pending new-worker request).

    def _issue_task(self):
        # Called from the event loop to issue a task to an idle worker and record in pending_ids
        # what to do when the task completes.
        item = self._items.get()
        wrkr = self._workers.pop()
        if isinstance(item.weights, Weights):
            if id(item.weights) not in self._weight_id_map:
                self._weight_id_map[id(item.weights)] = (ray.put(item.weights),
                    weakref.finalize(item.weights, self._weight_id_map.pop, id(item.weights)))
            # Pass the ray ObjectID for the weights directly - Ray will call the
            # function on the remote worker with an actual (python) weights object.
            weights, _ = self._weight_id_map[id(item.weights)]
        else:
            weights = item.weights # int seed for recreation/initialization
        issue_time = time.time()
        timeout_len = self._timeout_lengths.setdefault(item, self._default_timeout)
        result_id, logs_id = wrkr.exec_return_logs.remote(weights, item.logging_scope, item.func)
        self._timeouts[result_id] = (issue_time, item, wrkr)
        print("Issued (from {} tasks) {} to {} (among {} idle) weights {} timeout {}".format(
            len(self._items) + 1, item.name, wrkr.desc, len(self._workers)+1, id(item.weights), timeout_len))
        # Define the procedure for handling the logs when we receive them.
        # Note we'll only ask Ray for the logs after we've retrieved a successful result
        def try_handle_logs():
            try:
                logs_recvd = ray.get(logs_id, 0.001)
            except (ray.exceptions.RayActorError, ray.exceptions.UnreconstructableError) as e:
                # UnreconstructableError indicates the logs were produced, but lost in transit.
                # (It's not clear RayActorError can ever be thrown here, given another return-value
                # of the same method call has already been retrieved.)
                # Typically this means that the worker machine has been pre-empted after completing the task
                # but before transmitting all the logs back. We will rerun the task to get the logs...
                print("Note: lost logs for {} from {} due to {}; rerunning task".format(item.name, wrkr.desc, e))
                # ...but discard the result of the rerun (as we do not have a mechanism to rerun dependent tasks).
                # Assuming all tasks are deterministic, the logs will be the same as those lost from the first attempt.
                self._items.put(item.urgency, WorkerPool.WorkItem(
                    item.urgency, item.name + "_rerun", item.weights, item.func,
                    continuation=lambda _:None,
                    logging_scope=item.logging_scope))
                return
            # Got the logs. Enqueue a local task to dump them to disk/etc., of lower urgency than any continuation
            self._local_tasks.put((0, item.urgency), (f"dump logs for {item.name}", self._dump_logs, item.time_acc, logs_recvd))
        def try_handle_result():
            self._timeouts.pop(result_id)
            try:
                # The second argument here is the timeout, in seconds, after which an exception is thrown.
                # However timeout of 0 means wait indefinitely! So we use 0.001 just to throw rather than block.
                res, weight_load_time, wrkr_task_time = ray.get(result_id, 0.001)
                wrkr.used = True
            except (ray.exceptions.RayActorError, ray.exceptions.UnreconstructableError) as e:
                # Result lost in transit. We need the result, so must reissue the task.
                self._worker_died(wrkr, item, time.time(), issue_time, str(e))
                return
            except ray.exceptions.RayTaskError as e:
                # See if the exception was thrown in the creation of the Tensorflow model.
                # (The contents of the RayTaskError seem a bit weird, no "cause" or args)
                if type(e.cause_cls()) == NoGPUError and wrkr.used is False:
                    # On first try, Worker failed to create the Tensorflow model, seems it's on a machine with a bad GPU.
                    # We have to keep a Worker object (Ray Actor) on the node:
                    # otherwise Ray will just keep trying to instantiate another.
                    self._unusable_workers.append(wrkr)
                    self._worker_died(wrkr, item, time.time(), issue_time, str(e))
                    return
                else:
                    # Some other error in the task (our code), or a GpuInitializationError
                    # on a node that has already created a model successfully once
                    # (we don't think happens - machines are bad from the start, or stay good throughout)
                    raise e
            if item.time_acc is not None:
                item.time_acc.add(wrkr_task_time)
            driver_time = time.time() - issue_time
            analytics.event("ray_recv_result", taskname=item.name, worker=wrkr.desc,
                weight_load_time=weight_load_time,
                wrkr_task_time=wrkr_task_time,
                transmit_time=driver_time - weight_load_time - wrkr_task_time)
            self._workers.append(wrkr)
            # Result successfully retrieved - now start asking Ray for the logs
            self._pending_ids[logs_id] = ("logs:" + item.name, try_handle_logs)
            # Execute continuation later after issuing more work. Use the urgency of the original item, higher than log-dumping.
            self._local_tasks.put((1, item.urgency), (f"continuation for {item.name}", item.run_continuation, item.time_acc, res))
        # Start asking Ray for the result of the task (but not the logs, yet)
        self._pending_ids[result_id] = (item.name, try_handle_result)

    def _worker_counts(self):
        return {"num_workers": self._num_workers, "unusable_workers": len(self._unusable_workers)}

    def _issue_new_worker_request(self):
        # Called from the event loop to request Ray to make a new worker, and record in pending_ids
        # how to deal with Ray ever finding resources with which to realize the request (e.g. a new machine).
        print("Trying to make worker {}".format(self._next_worker_idx))
        new_wrkr = self._worker_class.remote(self._base_config, self._next_worker_idx)
        # Note 'new_wrkr' always returns an Actor handle, even before Ray has found a machine on which to instantiate the Actor.
        new_wrkr.desc = "Worker#{}".format(self._next_worker_idx) # Unlike _idx, this field lives on the driver-side proxy object
        new_req = new_wrkr.get_node_desc.remote()
        # The worker has only been instantiated on a host if that method returns.
        self._next_worker_idx += 1
        def try_handle_worker_created():
            try:
                # If ray.get() returns then we have a new worker. Note it hasn't created a Tensorflow GNN yet.
                desc = ray.get(new_req, 0.001)
                new_wrkr.desc = desc
                new_wrkr.used = False
                self._workers.append(new_wrkr)
                self._num_workers += 1
                analytics.event("worker_joined", time=(time.time() - self._start_time), worker=new_wrkr.desc, **self._worker_counts())
            except ray.exceptions.RayActorError as e:
                # Worker must have died even before we were able to get its description (!)
                print("{} failed before first use because {}.".format(new_wrkr.desc, e))
            # Either way, make sure we have a request pending
            self._issue_new_worker_request()
        self._pending_ids[new_req] = ("<new-worker>", try_handle_worker_created)

    def run(self):
        if self._start_time is not None:
            raise ValueError("Already running")
        self._start_time = time.time()
        self._num_workers = len(self._workers)

        with contextlib.ExitStack() as stack:
            # All these files will be closed when we exit this scope.
            self._worker_events_files = [stack.enter_context(
                    utils.open_file_mkdir(
                        os.path.join(self._worker_events_dir, analytics.events_filename(i)), "wb"))
                for i in range(analytics.DEFAULT_VERBOSITY_LIMIT + 1)]
            self._run_inner()

    def _run_inner(self):
        # From now on that we are running, self._workers contains only the IDLE workers
        analytics.event("run_start", time=0, **self._worker_counts())

        # Keep a request for a new Worker pending, so if any node connects, Ray will instantiate the Worker on it.
        self._issue_new_worker_request()

        # Main event loop.
        wait_time, num_idle_workers = None, 0
        def work_to_do():
            # There is always a new-worker request pending, but this doesn't count as work!
            assert len(self._pending_ids) >= 1
            if self._items.empty() and self._local_tasks.empty() and len(self._pending_ids) == 1:
                name, _ = utils.single_elem(self._pending_ids.values())
                assert name == "<new-worker>"
                return False
            return True
        while work_to_do():
            now = time.time()
            if wait_time is not None and num_idle_workers>0:
                analytics.event("workers_idle", secs=(now - wait_time) * len(self._workers))
            wait_time = now
            # First priority - keep the workers busy.
            while len(self._workers) > 0 and not self._items.empty():
                self._issue_task()
            num_idle_workers = len(self._workers)
            # Now wait for something to complete.
            # Limit timeout as even Ctrl-C ignored during ray.wait; but if we have continuations to run, only poll.
            timeout = 10.0 if self._local_tasks.empty() else 0.0
            remote_completed,_ = ray.wait(list(self._pending_ids), num_returns=1, timeout=timeout)
            if timeout > 0.0:
                print("Waited at t={}s for {}s with {} tasks queued, {} live weights, {} tasks running: {}".format(
                    round(wait_time - self._start_time, 1),
                    round(time.time() - wait_time, 1),
                    len(self._items),
                    len(self._weight_id_map),
                    len(self._pending_ids),
                    [name for name,_ in self._pending_ids.values()]))
            else:
                print("Poll")
            if len(remote_completed) > 0:
                res_id = utils.single_elem(remote_completed)
                # This'll handle either a new worker or a completed workitem (or a failed one!)
                _, func = self._pending_ids.pop(res_id)
                func()
                # Loop round to issue work to the new(ly-idle) worker ASAP before we execute continuations.
            elif not self._local_tasks.empty():
                # All workers busy (or no tasks to issue), and no tasks complete.
                name, *rest = self._local_tasks.get()
                print(f"Invoking local {name}")
                taken = measure_time(*rest)
                if taken > 5 or self._local_task_limit > 0:
                    print("Note: {} took {}s".format(name, round(taken, 1)))
                    if self._local_task_limit > 0 and taken > self._local_task_limit:
                        raise Exception(f"Local task {name} took {taken}s, exceeding limit of {self._local_task_limit}")
            else:
                # Check for timed-out tasks
                for r_id, (issue_time, item, wrkr) in list(self._timeouts.items()): # List to avoid removal during iteration
                    if issue_time + self._timeout_lengths[item] < now:
                        print(f"Timing out {item.name} on worker {wrkr.desc}")
                        self._timeouts.pop(r_id)
                        self._timeout_lengths[item] *= 2
                        # It may be that a _RemoteWorker is still working on the task, and it's just taking too long.
                        # If so, the following makes that _RemoteWorker free up its resources, in which case its host (body)
                        # will then have resources free to satisfy the pending new-worker request and we'll get a new worker.
                        ray.kill(wrkr)
                        # If instead the _RemoteWorker in question has gone AWOL with loss of connection, then our remote call
                        # may never return (even to throw an exception). So, stop listening, and assume the worker is already dead.
                        self._pending_ids.pop(r_id)
                        self._worker_died(wrkr, item, now, issue_time, None)

        # All tasks complete, nothing left to issue.
        analytics.event("run_finished", time=(time.time() - self._start_time), **self._worker_counts())
        self._start_time = None

@ray.remote
class _RemoteWorker(WorkerWithModel):
    ### The ray.remote "actor" that sits on the remote machine to manage the resource (Tensorflow ModelWrapper) there.

    def __init__(self, config, idx):
        super().__init__(config)
        if config['force_gpu']:
            gpu_id = utils.single_elem(ray.get_gpu_ids())
            assert str(gpu_id) == os.environ['CUDA_VISIBLE_DEVICES']
            suffix = "GPU{}".format(gpu_id)
        else:
            suffix = "CPU"
        machine_desc = [ray.services.get_node_ip_address()]
        for k in ["AZ_BATCH_TASK_ID", "AZ_BATCH_NODE_ID"]:
            v = os.environ.get(k)
            if v is not None:
                machine_desc.append(v)
        self._node_desc = "Worker#{}/{}@{}".format(idx, suffix, ",".join(machine_desc))
        self._idx = idx
        self._logfile_prefix = os.path.join(config['result_save_path'], str(idx), "")
        self._tasks_executed = 0
        self._kill_after_tasks = config['test_kill_worker_after_tasks']

    # This enables the driver node to get the IP where a Worker has been allocated (by Ray).
    def get_node_desc(self):
        return self._node_desc

    @ray.method(num_return_vals=2)
    def exec_return_logs(self, weights, logging_scope, func):
        if self._tasks_executed == self._kill_after_tasks:
            os.system("ray stop")
            # Minor race condition here - it's not clear how quickly the spawned ray-killing task will take
            # and whether we might get to the end of this method before our process dies.
        print("{} executing task {}, kill at {}".format(self._node_desc, self._tasks_executed, self._kill_after_tasks))
        self._tasks_executed += 1

        # Use the worker's idx in case there are multiple Worker objects sharing a machine.
        with logging_scope, analytics.log_events_to_files(f"{self._idx}/", overwrite=True):
            res_and_times = super().exec(weights, func)
            res, *_ = res_and_times
        # This diagnostic only appears on the console if we call initialize ray with --log_to_driver
        print("{} trying to return {}".format(self.get_node_desc(), res))
        logs = []
        for i in range(analytics.DEFAULT_VERBOSITY_LIMIT + 1):
            fname = os.path.join(str(self._idx), analytics.events_filename(i))
            with open(fname, "rb") as f:
                logs.append(f.read())
        return res_and_times, logs
