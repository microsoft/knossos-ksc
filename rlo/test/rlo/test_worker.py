import functools

import numpy as np
import pytest

from rlo import analytics
from testutils import get_config, parse_expr_typed
from rlo.local_worker import LocalWorker
from rlo import utils
from rlo import worker

exp = parse_expr_typed("(let (a (div 1.0 x)) (div a (add 1.0 a)))")


def eval_exp(model_wrapper, msg):
    return (msg, utils.single_elem(model_wrapper.evaluate_all_time_left([exp])))


def test_local_worker():
    results = []

    def handle_res(res):
        results.append(res)
        msg, _vals = res
        w.schedule(
            "ev h " + msg,
            None,
            functools.partial(eval_exp, msg="high after " + msg),
            results.append,
            urgency=1,
        )
        w.schedule(
            "ev l " + msg,
            None,
            functools.partial(eval_exp, msg="low after " + msg),
            results.append,
            urgency=0,
        )

    config = get_config()
    config["device"] = "/cpu:0"  # closer to deterministic
    w = LocalWorker(config)
    w.schedule(
        "ev h", None, functools.partial(eval_exp, msg="high"), handle_res, urgency=1,
    )
    w.schedule(
        "ev l", None, functools.partial(eval_exp, msg="low"), handle_res, urgency=0,
    )
    w.run()
    assert len(results) == 6
    _, vals = results[0]
    for _, v in results[1:]:
        np.testing.assert_allclose(v, vals, rtol=1e-6)
    # This ordering requires that the scheduler is FIFO at any given priority level
    assert [msg for msg, _val in results] == [
        "high",
        "high after high",
        "low",
        "high after low",
        "low after high",
        "low after low",
    ]


def test_local_worker_with_clients():
    results1, results2 = [], []

    def client1():
        s, v = yield worker.RunOnGPU(
            "footask", None, functools.partial(eval_exp, msg="foo")
        )
        assert s == "foo"
        results1.append(v)
        (s2, v2), (s3, v3) = yield worker.RunMultiple(
            [
                worker.RunOnGPU(
                    "bartask", None, functools.partial(eval_exp, msg="bar"),
                ),
                worker.RunOnGPU(
                    "baztask", None, functools.partial(eval_exp, msg="baz"),
                ),
            ]
        )
        assert s2 == "bar"
        assert s3 == "baz"
        results1.append(v2)
        results1.append(v3)

    def client2():
        (x, vx), (y, vy) = yield worker.RunMultiple(
            [
                worker.RunOnGPU("x_task", None, functools.partial(eval_exp, msg="x")),
                worker.RunOnGPU("y_task", None, functools.partial(eval_exp, msg="y")),
            ]
        )
        assert x == "x"
        assert y == "y"
        results2.append(vx)
        results2.append(vy)

    w = LocalWorker(get_config())
    w.schedule_work_requests_from(client1())
    w.schedule_work_requests_from(client2())
    w.run()
    assert len(results1) == 3  # client1 completed
    assert len(results2) == 2  # client2 completed


def eval_log(model_wrapper, msg):
    analytics.event(msg)
    return utils.single_elem(model_wrapper.evaluate_all_time_left([exp]))


def test_local_worker_with_client_logging():
    bar_result = []

    def foo_client():
        yield worker.RunOnGPU("taskf1", None, functools.partial(eval_log, msg="foo"))
        yield worker.RunOnGPU(
            "taskf2",
            None,
            functools.partial(eval_log, msg="foo2"),
            scope=analytics.Scope(inner=1),
        )
        yield worker.RunOnGPU("taskf3", None, functools.partial(eval_log, msg="foo3"))

    def bar_client():
        sc2 = analytics.Scope(inner=2)
        v = yield worker.RunOnGPU(
            "taskb1", None, functools.partial(eval_log, msg="bar"), scope=sc2,
        )
        v2 = yield worker.RunOnGPU(
            "taskb2", None, functools.partial(eval_log, msg="bar2")
        )
        v_sum = v[3] + v2[3]
        bar_result.append(v_sum)
        yield worker.RunOnGPU(
            "taskb3",
            None,
            functools.partial(eval_log, msg="bar3"),
            scope=analytics.Scope(sc2, v_sum=v_sum),
        )

    w = LocalWorker(get_config())
    with analytics.LogEventsToList() as l:
        with analytics.Scope(outer=1):
            w.schedule_work_requests_from(foo_client())
        with analytics.Scope(outer=2):
            w.schedule_work_requests_from(bar_client())
        w.run()
    assert sorted(l.log_items, key=lambda e: e["event"]) == [
        {"event": "bar", "outer": 2, "inner": 2},
        {"event": "bar2", "outer": 2},
        {
            "event": "bar3",
            "outer": 2,
            "inner": 2,
            "v_sum": utils.single_elem(bar_result),
        },
        {"event": "foo", "outer": 1},
        {"event": "foo2", "outer": 1, "inner": 1},
        {"event": "foo3", "outer": 1},
    ]


def test_local_worker_as_continuation():
    results = []

    @worker.as_continuation
    def foo_cont(tup):
        s, res = tup
        assert s == "foo"
        results.append(res)
        s, res = yield worker.RunOnGPU(
            "testbar", None, functools.partial(eval_exp, msg="bar")
        )
        assert s == "bar"
        results.append(res)

    w = LocalWorker(get_config())
    w.schedule(
        "testfoo", None, functools.partial(eval_exp, msg="foo"), foo_cont(w),
    )
    w.run()
    assert len(results) == 2


def test_local_worker_stackoverflow_without_wait():
    def foo(n, wait):
        for i in range(n):
            r = yield worker.RunOnGPU(
                f"test{i}", None, lambda model_wrapper: 3, wait_for_result=False
            )
            assert r is None  # result (3) not returned
            if wait:
                r = yield worker.RunOnGPU(f"testwait{i}", None, lambda model_wrapper: 4)
                assert r == 4

    w = LocalWorker(get_config())
    w.schedule_work_requests_from(foo(10, False))
    w.run()
    with utils.override_recursion_limit(300):
        with pytest.raises(RecursionError):
            w.schedule_work_requests_from(foo(1000, False))
    w.run()
    w.schedule_work_requests_from(foo(2000, True))
