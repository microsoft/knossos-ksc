from contextlib import contextmanager
import os
import shutil
from tempfile import TemporaryDirectory


def get_base_docker_image(docker_file):
    """ Returns the name of the base image given a path to a docker file.
    """
    with open(docker_file) as f:
        from_line = next(
            line for line in f.read().split("\n") if line.startswith("FROM")
        )
    _from, base_image = from_line.split()
    return base_image


@contextmanager
def combined_source_directory():
    root_dir = os.path.dirname(os.path.dirname(__file__))
    with TemporaryDirectory() as tmpdir:
        print("Copying sources to staging directory", tmpdir)
        # This puts the contents of src directly into tmpdir
        shutil.copytree(
            os.path.join(root_dir, "src"),
            tmpdir,
            ignore=shutil.ignore_patterns("__pycache__"),
            dirs_exist_ok=True,  # tmpdir exists already
        )
        # Copy ksc in as a subdirectory. Note this does not include ts2ks.
        shutil.copytree(
            os.path.join(root_dir, "knossos-ksc", "src", "python", "ksc"),
            os.path.join(tmpdir, "ksc"),
        )
        yield tmpdir
