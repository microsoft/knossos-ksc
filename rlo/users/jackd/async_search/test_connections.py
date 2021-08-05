import numpy as np
from queue import Queue
import connections as con


def test_pipe():
    client_con, server_con = con.pipe(Queue)

    assert not client_con.poll()
    assert not server_con.poll()

    client_con.send(2)
    client_con.send(5)

    assert not client_con.poll()
    assert server_con.poll()

    r0 = server_con.recv()
    assert server_con.poll()
    assert r0 == 2
    r1 = server_con.recv()
    assert not server_con.poll()
    assert r1 == 5

    server_con.send(r0 ** 2)
    server_con.send(r1 ** 2)
    assert client_con.poll()
    assert client_con.recv() == 2 ** 2
    assert client_con.poll()

    assert client_con.recv() == 5 ** 2
    assert not client_con.poll()


def test_forked():
    num_clients = 2
    forked, clients = con.forked_pipe(Queue, num_clients)

    clients[0].send(3)
    clients[1].send(5)
    clients[1].send(7)
    clients[0].send(2)

    assert forked.num_waiting() == 4
    assert forked.poll()

    values = []
    while forked.poll():
        values.append(forked.recv())

    mapped_values = np.array(values) ** 2  # could be an expensive remote operation

    for mapped_value in mapped_values:
        forked.send(mapped_value)

    assert clients[0].num_waiting() == 2
    assert clients[1].num_waiting() == 2
    assert clients[0].recv() == 9  # 3 ** 2
    assert clients[0].recv() == 4  # 2 ** 2
    assert clients[1].recv() == 25  # 5 ** 2
    assert clients[1].recv() == 49  # 7 ** 2

    assert not forked.poll()
    assert not clients[0].poll()
    assert not clients[1].poll()


def test_pass_through():
    c0, c1 = con.pipe(Queue)
    c1 = con.PassThroughConnection(c1)

    c0.send(("a", 3))
    c0.send(("b", 5))

    assert c1.num_waiting() == 2
    values = [c1.recv() for _ in range(2)]
    assert values == [3, 5]

    for v in values:
        c1.send(v ** 2)

    assert not c1.poll()
    assert c0.num_waiting() == 2
    assert c0.recv() == ("a", 9)
    assert c0.recv() == ("b", 25)


def test_fifo_connected_fn():
    c0, c1 = con.pipe(Queue)
    fn = con.FifoConnectedFunction(c0)

    f0 = fn(3)
    f1 = fn(5)

    assert c1.num_waiting() == 2
    values = [c1.recv() ** 2 for _ in range(c1.num_waiting())]
    for v in values:
        c1.send(v)

    fn.flush()

    assert f0.done()
    assert f0.result() == 9
    assert f1.done()
    assert f1.result() == 25


def test_unordered_connected_fn():
    c0, c1 = con.pipe(Queue)
    fn = con.UnorderedConnectedFunction(c0)

    f0 = fn(3)
    f1 = fn(5)

    assert c1.num_waiting() == 2
    for uid, val in [c1.recv() for _ in range(c1.num_waiting())][-1::-1]:
        c1.send((uid, val ** 2))

    fn.flush()

    assert f0.done()
    assert f0.result() == 9
    assert f1.done()
    assert f1.result() == 25


def test_unordered_pass_through():
    # single client, multiple servers
    client_conn, server_conn = con.pipe(Queue)
    server_conns = [con.PassThroughConnection(server_conn.copy()) for _ in range(2)]

    client_fn = con.UnorderedConnectedFunction(client_conn)

    futures = [client_fn(v) for v in [3, 5, 7, 11]]

    values = [
        server_conns[1].recv(),
        server_conns[0].recv(),
        server_conns[0].recv(),
        server_conns[1].recv(),
    ]

    values = [v ** 2 for v in values]
    server_conns[0].send(values[1])
    server_conns[1].send(values[0])
    server_conns[1].send(values[3])
    server_conns[0].send(values[2])

    client_fn.flush()
    assert all([f.done() for f in futures])
    assert [f.result() for f in futures] == [9, 25, 49, 121]


def test_forked_fifo_clients():
    # multiple clients, single server
    server_conn, client_conns = con.forked_pipe(Queue, 2)
    client_fns = [con.FifoConnectedFunction(cc) for cc in client_conns]

    futures = [
        client_fns[0](3),
        client_fns[1](5),
        client_fns[0](7),
        client_fns[1](11),
    ]

    assert server_conn.num_waiting() == 4
    values = [server_conn.recv() for _ in range(server_conn.num_waiting())]
    for v in values:
        server_conn.send(v ** 2)

    for fn in client_fns:
        fn.flush()

    assert all([f.done() for f in futures])
    assert [f.result() for f in futures] == [9, 25, 49, 121]


def test_forked_unordered_pass_through():
    # multiple clients, multiple servers
    num_clients = 2
    num_servers = 2
    server_conn, client_conns = con.forked_pipe(Queue, num_clients)
    client_fns = [
        con.UnorderedConnectedFunction(client_conn) for client_conn in client_conns
    ]
    server_conns = [
        con.PassThroughConnection(server_conn.copy()) for _ in range(num_servers)
    ]

    futures = [
        client_fns[0](3),
        client_fns[1](5),
        client_fns[0](7),
        client_fns[1](11),
    ]

    values = [
        server_conns[1].recv(),
        server_conns[0].recv(),
        server_conns[0].recv(),
        server_conns[1].recv(),
    ]

    values = [v ** 2 for v in values]
    server_conns[0].send(values[1])
    server_conns[1].send(values[0])
    server_conns[1].send(values[3])
    server_conns[0].send(values[2])

    for fn in client_fns:
        fn.flush()
    assert all([f.done() for f in futures])
    assert [f.result() for f in futures] == [9, 25, 49, 121]


def test_get_connected_functions_ordered_single_client():
    (fn,), c1 = con.get_connected_functions(Queue, 1, True)
    f0 = fn(3)
    f1 = fn(5)

    assert c1.num_waiting() == 2
    values = [c1.recv() ** 2 for _ in range(c1.num_waiting())]
    for v in values:
        c1.send(v)

    fn.flush()

    assert f0.done()
    assert f0.result() == 9
    assert f1.done()
    assert f1.result() == 25


def test_get_connected_functions_ordered_multi_client():
    client_fns, server_conn = con.get_connected_functions(Queue, 2, True)

    futures = [
        client_fns[0](3),
        client_fns[1](5),
        client_fns[0](7),
        client_fns[1](11),
    ]

    assert server_conn.num_waiting() == 4
    values = [server_conn.recv() for _ in range(server_conn.num_waiting())]
    for v in values:
        server_conn.send(v ** 2)

    for fn in client_fns:
        fn.flush()

    assert all([f.done() for f in futures])
    assert [f.result() for f in futures] == [9, 25, 49, 121]


def test_get_connected_functions_unordered_single_client():
    (client_fn,), server_conn = con.get_connected_functions(Queue, 1, False)
    server_conns = [server_conn, server_conn.copy()]

    futures = [client_fn(v) for v in [3, 5, 7, 11]]

    values = [
        server_conns[1].recv(),
        server_conns[0].recv(),
        server_conns[0].recv(),
        server_conns[1].recv(),
    ]

    values = [v ** 2 for v in values]
    server_conns[0].send(values[1])
    server_conns[1].send(values[0])
    server_conns[1].send(values[3])
    server_conns[0].send(values[2])

    client_fn.flush()
    assert all([f.done() for f in futures])
    assert [f.result() for f in futures] == [9, 25, 49, 121]


def test_get_connected_functions_unordered_multi_client():
    client_fns, server_conn = con.get_connected_functions(Queue, 2, False)
    server_conns = [server_conn, server_conn.copy()]

    futures = [
        client_fns[0](3),
        client_fns[1](5),
        client_fns[0](7),
        client_fns[1](11),
    ]

    values = [
        server_conns[1].recv(),
        server_conns[0].recv(),
        server_conns[0].recv(),
        server_conns[1].recv(),
    ]

    values = [v ** 2 for v in values]
    server_conns[0].send(values[1])
    server_conns[1].send(values[0])
    server_conns[1].send(values[3])
    server_conns[0].send(values[2])

    for fn in client_fns:
        fn.flush()
    assert all([f.done() for f in futures])
    assert [f.result() for f in futures] == [9, 25, 49, 121]
