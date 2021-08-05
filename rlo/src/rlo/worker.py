# fmt: off
from abc import ABC, abstractmethod
from typing import Any, Callable, Generator, Iterable, Optional, Union

from rlo import analytics
from rlo.utils import UrgencyQueue

class Accumulator:
    """ A simple mutable holder of a float that allows addition, i.e. the value can only increase. """
    def __init__(self, initial_value: float=0.0):
        self._total = initial_value

    def add(self, amount: float) -> None:
        assert amount >= 0.0
        self._total += amount

    @property
    def value(self) -> float:
        return self._total

class WorkerPool(ABC):
    """ Interface for asynchronously executing work with a model (locally or remotely).
        Provides a simple priority control. """
    def __init__(self):
        self._items = UrgencyQueue() # data are WorkItem's
        self._item_count = 0

    class WorkItem:
        def __init__(self, urgency, name, weights, func, continuation,
                     logging_scope=None, cont_logging_scope=None, time_acc=None):
            self.urgency = urgency
            self.name = name
            self._cont = continuation
            self.weights = weights
            self.func = func
            self.logging_scope = analytics.get_current_scope() if logging_scope is None else logging_scope
            self.cont_logging_scope = self.logging_scope if cont_logging_scope is None else cont_logging_scope
            self.time_acc = time_acc

        def run_continuation(self, res):
            with self.cont_logging_scope:
                self._cont(res)

    def schedule(self, name, weights: Union["model.Weights",  # type: ignore[name-defined]
                                            int, None], func, continuation=lambda *_: None,
                 urgency=0, logging_scope=None, cont_logging_scope=None, time_acc=None):
        """ Schedules 'func' to be executed, perhaps on remote system, with a model with the prescribed weights loaded.
            By default func will be executed in the current logging scope (i.e. at schedule-time),
                but this may be overridden by the 'logging_scope' parameter.
            When 'func' returns a result, continuation(result) will be called on the local system,
                in the same scope as func unless cont_logging_scope is provided.

            func should be pure/stateless, and indeed to run over Ray, func must refer only to immutable objects.
            (func may mutate/train the model, but if so, should return the weights so they can be passed to the continuation)
            It will be passed a ModelWrapper into which 'weights' have been loaded; alternatively weights may be an int
            to indicate the model should be reinitialised using that random seed, or None to avoid loading (useful in tests).

            The default continuation does nothing (ignores the result).
            Highest urgency first as far as possible.

            Time (in seconds) spent in both <func> and <continuation> will be added to time_acc if not None. """
        # Do not execute function here; wait until we get into run.
        # (We may be called from inside a continuation, so let it schedule all work at all priorities, until we return back to run())
        # The -item_count ensures that for the same urgency parameter, the urgency of the Items gets lower, i.e. FIFO
        urgency = (urgency, -self._item_count)
        self._items.put(urgency, WorkerPool.WorkItem(urgency, name, weights, func, continuation, logging_scope=logging_scope,
                                                     cont_logging_scope=cont_logging_scope, time_acc=time_acc))
        self._item_count +=1

    @abstractmethod
    def run(self):
        """ Runs scheduled work until nothing is scheduled and the last continuation has returned. """

    def schedule_work_requests_from(self, generator_fn: Generator["WorkRequest", Any, None]):
        """ Runs <generator_fn> until it yields its first WorkRequest, which is then scheduled;
            execution of <generator_fn> is then paused, and schedule_work_requests_from returns.
            When pool.run() is called, the scheduled WorkRequest will be executed (perhaps remotely),
            and when the result is available it will be passed back using `iterator.send`, such that
            <generator_fn> then resumes execution until it yields another WorkRequest.
            <generator_fn> allows a client (repetition or flow of work) to be expressed as follows:
                def my_client():
                    ....
                    res1 = yield RunOnGPU(....)
                    ....
                    res2, res3 = yield RunMultiple([RunOnGPU(...), RunOnGPU(...)])
                pool.schedule_work_requests_from(my_client())
            HOWEVER note there is a restriction on such client generators, as follows:
              they MUST NOT yield WITHIN any analytics scope (no "with Scope: ...yield...")
        """
        def go(res):
            # Make sure the generator didn't yield inside "with", as doing so leads to a
            # right hard-to-debug mess.
            backup_scopes = analytics._scopes[:]
            try:
                request:"WorkRequest" = generator_fn.send(res)
            except StopIteration:
                return
            assert backup_scopes == analytics._scopes, "Generator function yielded inside with"
            request.schedule_on_pool(self, go)
        go(None)

class WorkRequest(ABC):
    @abstractmethod
    def schedule_on_pool(self, pool: WorkerPool, cont: Callable):
        """ Schedules this WorkRequest onto the specified pool
            such that cont will be executed with the result when finished. """

class RunOnGPU(WorkRequest):
    def __init__(self, name: str, weights, func: Callable,
                 urgency=0,
                 time_acc: Optional[Accumulator]=None,
                 scope: Optional[analytics.Scope]=None,
                 wait_for_result=True):
        """ A WorkRequest that <func> be executed with one argument: a model that has <weights> loaded.
            If <time_acc> is not None, the time taken (remotely) will be added.
            If <wait_for_result> is true, the result of yielding this (from a client generator) is the result returned by <func>.
            If <wait_for_result is false, the result of yielding this will be None: the 'yield' will return immediately,
                and <func> will be executed in the background.
            <scope> allows the logging scope to be explicitly specified, as generators may not yield
                 inside "with Scope"; otherwise the current scope
                 (captured at time of constructing the RunOnGPU object) is used. """
        self.name = name
        self.weights = weights
        self.func = func
        self.urgency = urgency
        self.time_acc = time_acc
        self.scope = analytics.get_current_scope() if scope is None else scope
        self._wait = wait_for_result

    def schedule_on_pool(self, pool: WorkerPool, cont: Callable):
        # The scope for the continuation is that in which the generator is running at the point where this
        # WorkRequest is _yielded_, which (schedule_work_requests_from ensures) must be the same scope
        # as in which the generator was first started, NOT the scope in which the remote WorkItem runs.
        cont_scope = analytics.get_current_scope()
        with self.scope:
            pool.schedule(self.name, self.weights, self.func, continuation=(cont if self._wait else lambda _:None),
                          urgency=self.urgency, cont_logging_scope=cont_scope, time_acc=self.time_acc)
        if not self._wait:
            # Note this typically recurses into the next go() which then calls schedule_on_pool, so recursion
            # depth increases without limit until the generator yields something it *does* want to wait for.
            cont(None)

class RunMultiple(WorkRequest):
    def __init__(self, items:Iterable[WorkRequest]):
        """ A WorkRequest that all the requests in <items> be executed (perhaps in parallel).
            The result of yielding this (from a client generator) is a list of the results of each item. """
        self.items = tuple(items)
    def schedule_on_pool(self, pool, cont):
        results = [None] * len(self.items)
        def got(res, n):
            results[n] = res
        rc = WhenAllResultsReceived(len(self.items), got, lambda: cont(results))
        for idx, item in enumerate(self.items):
            item.schedule_on_pool(pool, rc.continuation_for_result(idx))

def as_continuation(generator_fn):
    """ Used as a decorator to turn a generator_fn that yields WorkRequests
        into a continuation that can be passed to pool.schedule in CPS style:
            @worker.as_continuation
            def foo(args):
                ...
                yield WorkRequest(...)
                ...
            pool.schedule(...., continuation=foo(pool))
    """
    return lambda pool: lambda *args, **kwargs: pool.schedule_work_requests_from(generator_fn(*args, **kwargs))

class ComposableFunction:
    """ A callable that allows composition.

    For example,
    >>> ComposableFunction(opA).then(opB).then(opC)(x)
    is equivalent to
    >>> opC(opB(opA(x)))

    Or with extra arguments:
    >>> ComposableFunction(opA).then(opB, arg1, arg2=arg2).then(opC, arg3)(x)
    is equivalent to:
    >>> opC(opB(opA(x), arg1, arg2=arg2), arg3)
    """
    def __init__(self, func):
        self._func = func

    def __call__(self, *args, **kwargs):
        """ Call the function """
        return self._func(*args, **kwargs)

    def then(self, cont, *additional_args, **additional_kwargs):
        """ Creates a new ComposableFunction using continuation """
        return ComposableFunction(
            lambda *args, **kwargs: cont(self(*args, **kwargs), *additional_args, **additional_kwargs)
        )

class AsTrainModel(ComposableFunction):
    """ An adapter function that interprets a ModelWrapper for training search.
    """
    def __init__(self):
        super().__init__(lambda model_wrapper: model_wrapper.as_train_search_model())

class AsEvalModel(ComposableFunction):
    """ An adapter function that interprets a ModelWrapper for evaluation search.
    """
    def __init__(self):
        super().__init__(lambda model_wrapper: model_wrapper.as_eval_search_model())


class WhenAllResultsReceived:
    """ A simple utility class for scheduling an continuation to be executed when several
        calls have all completed (regardless of the order in which they complete).
        Allows an action to be executed when each individual result arrives,
        as well as another when all have arrived.
        Note nothing is done to combine together the results - the continuation for each
        is expected to store in a data structure (list/hash/etc.) appropriate for the collection of results. """
    def __init__(self, num_expected_results:int, each_result_cont: Callable, all_results_cont: Callable):
        """ Creates a ResultsCombiner that will invoke <all_results_cont> (with no arguments)
            when it has received <num_expected_results> (via the `continuation_for_result` method).
            <each_result_cont> is a continuation which is invoked with each individual result. """
        self._each_result_cont = each_result_cont
        self._num_expected_results = num_expected_results
        self._num_continuations_issued = 0
        self._all_results_cont = all_results_cont
        self._results_received = 0


    def continuation_for_result(self, *args, **kwargs):
        """ Returns a continuation to receive an individual result, that calls <each_result_cont>
                with that result plus the specified <args> and <kwargs>; this is of form appropriate for e.g. WorkerPool.schedule.
            Each such continuation may be invoked at most once; if/when the number of results specified in the constructor
                have been received, then <all_results_cont> is then invoked.
        """
        assert self._num_continuations_issued < self._num_expected_results
        self._num_continuations_issued += 1
        this_result_received = False
        def receive_remote_result(res):
            nonlocal this_result_received
            assert not this_result_received
            this_result_received = True
            self._each_result_cont(res, *args, **kwargs)
            self._results_received += 1
            if self._results_received == self._num_expected_results:
                self._all_results_cont()
        return receive_remote_result
