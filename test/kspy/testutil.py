import importlib.util
from tempfile import NamedTemporaryFile

from kspy.translate import translate

def translate_and_import(*args):
    py_out = translate(*args)
    with NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(py_out)
    print(f.name)
    # These three lines are for loading a module from a file in Python 3.5+
    # https://bugs.python.org/issue21436
    spec = importlib.util.spec_from_file_location("py_out", f.name)
    py_out = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(py_out)
    return py_out
