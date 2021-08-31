import bz2
import json
import os
import tempfile

from rlo import analytics
from rlo.expression import Expression


def test_analytics_to_list():
    with analytics.LogEventsToList() as m:
        analytics.event("foo", num=3)
        with analytics.Scope(repetition=2):
            analytics.event("foo", num=4)
        analytics.event("bar")
    assert m.log_items == [
        {"event": "foo", "num": 3},
        {"event": "foo", "num": 4, "repetition": 2},
        {"event": "bar"},
    ]
    m._log_items.clear()
    analytics.event("baz")
    assert m.log_items == []


def test_capture_items_with_verbosity():
    with analytics.LogEventsToList(verbosity_limit=analytics.MAX_VERBOSITY) as m:
        analytics.event("foo")
        analytics.event("foooo", verbosity=1)
        analytics.event("foooooo", verbosity=2)
    captured = m.log_items_with_verbosity
    assert captured == [
        ({"event": "foo"}, 0),
        ({"event": "foooo"}, 1),
        ({"event": "foooooo"}, 2),
    ]


def test_scopes():
    with analytics.LogEventsToList() as m:
        with analytics.Scope(outer=1) as keep1:
            analytics.event("foo")
            with analytics.Scope(inner=1) as keep2:
                analytics.event("bar")
            with analytics.Scope(inner=2):
                analytics.event("baz")
                with analytics.Scope(keep2, extra=3):
                    analytics.event("barred")
                    with keep1:
                        analytics.event("food")
                    analytics.event("barred2")
                analytics.event("baz2")
            analytics.event("foo2")
        with analytics.Scope(outer=2):
            analytics.event("qux")
            with keep1:
                analytics.event("fooled")
                with keep2:
                    analytics.event("barricade")
                analytics.event("fooled2")
            analytics.event("qux2")
    assert m.log_items == [
        {"event": "foo", "outer": 1},
        {"event": "bar", "outer": 1, "inner": 1},
        {"event": "baz", "outer": 1, "inner": 2},
        {"event": "barred", "outer": 1, "inner": 1, "extra": 3},
        {"event": "food", "outer": 1},
        {"event": "barred2", "outer": 1, "inner": 1, "extra": 3},
        {"event": "baz2", "outer": 1, "inner": 2},
        {"event": "foo2", "outer": 1},
        {"event": "qux", "outer": 2},
        {"event": "fooled", "outer": 1},
        {"event": "barricade", "outer": 1, "inner": 1},
        {"event": "fooled2", "outer": 1},
        {"event": "qux2", "outer": 2},
    ]


def test_expressions_turn_to_str():
    x = Expression.Variable("x")
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpfile = os.path.join(tmpdir, "log.json.bz2")
        with analytics.log_events_to_file(tmpfile):
            analytics.event("outer", exp=(x * 2))
            with analytics.Scope(within=(x + 3)):
                analytics.event("inner")
        with bz2.open(tmpfile, "rt", encoding="utf-8") as f:
            items = [json.loads(line) for line in f]
    assert items == [
        {"event": "outer", "exp": "(mul x 2)"},
        {"event": "inner", "within": "(add x 3)"},
    ]
