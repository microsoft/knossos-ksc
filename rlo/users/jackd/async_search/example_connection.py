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


client_conn, server_conn = pipe(Queue)
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
