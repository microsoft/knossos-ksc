from contextlib import contextmanager
import os
import shutil
from tempfile import TemporaryDirectory


# extracted and adapted from RLO


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
