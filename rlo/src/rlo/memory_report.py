from contextlib import contextmanager
import psutil
import multiprocessing
import time

from rlo import utils


def report():
    while True:
        print(psutil.virtual_memory())
        time.sleep(60)


@contextmanager
def background_process(target):
    p = multiprocessing.Process(target=target)
    try:
        p.start()
        print(f"Starting background process {p}")
        yield p
    finally:
        p.terminate()
        while p.is_alive():
            time.sleep(1)
        print(f"Successfully terminated background process {p}")
        # Only in Python 3.7
        # p.close()


def memory_report():
    current_process = multiprocessing.current_process()
    if current_process.daemon:
        # Already inside a daemon process
        print("Cannot create a child process in a daemonic process")
        return utils.nullcontext()
    return background_process(report)
