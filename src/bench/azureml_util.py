from contextlib import contextmanager
import os
import shutil
from tempfile import TemporaryDirectory


# extracted and adapted from RLO

root_dir = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))


def copy_dir(target_dir, dir_name):
    shutil.copytree(
        os.path.join(root_dir, dir_name),
        os.path.join(target_dir, dir_name),
        ignore=shutil.ignore_patterns("__pycache__"),
        dirs_exist_ok=True,  # tmpdir exists already
    )


@contextmanager
def combined_source_directory():

    with TemporaryDirectory() as tmpdir:
        print("Copying sources to staging directory", tmpdir)
        copy_dir(target_dir=tmpdir, dir_name="src")
        copy_dir(target_dir=tmpdir, dir_name="examples")

        yield tmpdir
