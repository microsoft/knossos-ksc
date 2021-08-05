import glob
import json
import os
import pickle
import random
import sys
import time
from contextlib import contextmanager
from functools import wraps
from queue import PriorityQueue
from typing import (
    Callable,
    Dict,
    Hashable,
    Iterable,
    List,
    NamedTuple,
    Optional,
    Sequence,
    Set,
    Tuple,
    TypeVar,
    Union,
)

import numpy as np
import torch

K = TypeVar("K")
S = TypeVar("S")
T = TypeVar("T")

SMALL_NUMBER = 1e-7


@contextmanager
def override_recursion_limit(new_limit):
    old_limit = sys.getrecursionlimit()
    try:
        sys.setrecursionlimit(new_limit)
        yield None
    finally:
        sys.setrecursionlimit(old_limit)


class UrgencyQueue:
    """ Wrapper for a PriorityQueue, in which items are put() onto the queue with an explicit (separate) urgency parameter
        which determines the order in which they are popped off (highest urgency first). The data items are thus not compared. """

    def __init__(self):
        self._pq = PriorityQueue()

    def put(self, urgency, data):
        self._pq.put(self._HighestFirstItem(urgency, data))

    def get(self):
        return self._pq.get(block=False).data

    def empty(self):
        return self._pq.empty()

    def __len__(self):
        return self._pq.qsize()

    class _HighestFirstItem:
        def __init__(self, urgency, data):
            self._urgency = urgency
            self.data = data

        def __gt__(self, other):
            # PriorityQueue returns lowest first, so invert
            return self._urgency < other._urgency


@contextmanager
def nullcontext():
    try:
        yield None
    finally:
        pass


@contextmanager
def random_state_context(context_seed: int):
    """
    Set the numpy, torch, and python random state for the duration of the context.

    Example usage
    ```python
    np.random.seed(123)
    with random_state_context(234):
        # stuff in here doesn't affect the outer random state
        np.random.uniform()
    x = np.random.uniform()
    np.random.seed(123)
    assert x == np.random.uniform()
    ```
    
    If you really care about determinism then also do
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False
    (but this will make code run slower, according to docs)

    """

    numpy_state = np.random.get_state()
    py_state = random.getstate()

    with torch.random.fork_rng():
        try:
            np.random.seed(context_seed)
            random.seed(context_seed)
            torch.manual_seed(context_seed)
            yield
        finally:
            np.random.set_state(numpy_state)
            random.setstate(py_state)


def seed(random_source: Union[int, np.random.Generator]) -> int:
    """Get an integer seed from random_source."""
    # Force unsigned int for Windows.
    return (
        random_source
        if isinstance(random_source, int)
        else random_source.integers(0, 0x7FFFFFFF, dtype=int)
    )


def rng(random_source: Union[int, np.random.Generator]) -> np.random.Generator:
    """Get a random number generator, seeded from random_source"""
    return np.random.default_rng(seed(random_source))  # New in numpy 1.17


def torch_rng(seed_: int) -> torch.Generator:
    """Get a torch random number generator without advancing the state of the base random generator.
    Argument name has trailing underscore because we have a function called `seed` in this file too."""
    generator = torch.Generator()
    generator.manual_seed(seed_)
    return generator


def load_pickle(path):
    """Loads a file dumped by pickle"""
    with open(path, "rb") as f:
        return pickle.load(f)


def open_file_mkdir(path, mode="w"):
    """
    Opens the specified path in the given mode, after first
    creating the containing directory if it doesn't already exist
    """
    directory, _ = os.path.split(path)
    if not os.path.exists(directory):
        os.makedirs(directory)
    return open(path, mode)


def get_model_save_path(model_save_dir: str, gen_name: str):
    # TODO this should probably be "get_generation_dir"
    return os.path.join(model_save_dir, "generation_{}".format(gen_name))


def get_saved_model_files(model_save_dir: str, model_name="model") -> List:
    """ Returns a list of tuples of generation-number and model_file, in order of increasing generation """
    return sorted(
        [
            # Split off generation_ prefix to return just number, which is enough to sort all
            # of the tuples lexicographically (there is at most one model for each generation)
            (int(os.path.dirname(model_path).split("generation_", 1)[1]), model_path)
            for model_path in glob.glob(
                os.path.join(
                    get_model_save_path(model_save_dir, "*"), f"{model_name}_best.npz"
                )
            )
        ]
    )


def softmax(x: np.ndarray, alpha: float) -> np.ndarray:
    """Compute softmax values among the elements of 1d array x"""
    assert alpha >= 0  # Otherwise require protection against rounding to -Inf
    if np.isposinf(alpha):
        # Approximate by assigning probability 1 to maximum element(s), 0 to the rest
        exps = (x == np.max(x)) * 1.0
    else:
        exps = np.exp(alpha * (x - max(x)))
    return exps / np.sum(exps)


def count_occurances(virus, host):
    """
    This function counts the number of occurances of virus in the host
    TODO doesn't seem to be used: delete?
    """
    count = 0
    for c in host.nodes:
        if virus == c:
            count += 1
    return count


def print_no_newline(string):
    """Print with replacement without going to the new line
    Useful for showing the progress of training or search
    """
    sys.stdout.write(string)
    sys.stdout.flush()


def approximate_solutionspace_size(exp, rules, simulation_depth, n_trial):
    """
    This function takes an expression and compute the approximate size of the solution space.
    More n_trial results in an estimate with lower variance.
    """
    all_len_rules = []
    for trial in range(n_trial):
        e = exp
        prod = 1.0
        text_to_display = "Trial {} \r".format(trial)
        print_no_newline(text_to_display)
        for _ in range(simulation_depth):
            rws = list(rules.get_all_rewrites(e))
            if len(rws) == 0:
                break
            prod *= len(rws)
            e = random.choice(rws).apply(e)
        all_len_rules.append(prod)
    return np.average(all_len_rules)
    # print("Number of applicable rules at each stage {} which is total equals prod[{}] = {}".format(len_rules, len_rules, total_len_rules))


def print_rules(list_of_rules):
    """
    Print a list of rules line by line
    """
    for i, r in enumerate(list_of_rules):
        print("{}:{}".format(i, r))


def save_expression_as_tex(exp, dir):
    """
    Transforms exp into TEX format and saves it in dir
    """
    import matplotlib.pyplot as plt
    import sympy
    from sympy.core.evaluate import evaluate

    vars_in_exp = [node for node in exp.nodes if node.op == "variable"]
    for v in vars_in_exp:
        text = "{}=sympy.Symbol({})".format(v.__repr__(), "'{}'".format(v.__repr__()))
        exec(text)
    with evaluate(False):
        exp_symbol = eval(exp.__repr__())
    latex_exp = sympy.latex(exp_symbol)
    # Set the following size parameters if the expression does not fit in the axes
    xmin = 0
    xmax = 10
    ymin = 0
    ymax = 20
    _ = plt.axis(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)  # left,bottom,width,height
    yscale = ymax - ymin
    fontsize = yscale
    plt.text(0, ymax / 2, "$%s$" % latex_exp, fontsize=fontsize, usetex=True)
    plt.axis("off")
    plt.savefig(dir)


def group_snds_by_fst(lst: Iterable[Tuple[S, T]]) -> Dict[S, List[T]]:
    """
    Convert a list of tuples into a dictionary from first elements of the tuples to list of second
    elements encountered in the list.

    Example: [(a, 5), (b, 3), (a, 6)] -> {a: [5, 6], b: [3]}
    """
    d: Dict = dict()
    for k, v in lst:
        d.setdefault(k, []).append(v)
    return d


HashableTypeVar = TypeVar("HashableTypeVar", bound=Hashable)


def group_by(
    lst: Iterable[T], func: Callable[[T], HashableTypeVar]
) -> Dict[HashableTypeVar, List[T]]:
    """Groups elements in lst by the return value of function func.

    Args:
        lst: An iterable of elements
        func: A function that takes elements of lst as input, and returns some values that can be used
            as keys in a dictionary

    Returns:
        Returns a dictionary mapping from the values returned by func on the elements of lst,
        to the elements of lst which led to that value being returned
    """
    d: Dict[HashableTypeVar, List[T]] = dict()
    for l in lst:
        d.setdefault(func(l), []).append(l)
    return d


def single_elem(lst):
    if len(lst) != 1:
        raise ValueError("{} had {} elements but expected 1".format(lst, len(lst)))
    return next(iter(lst))


def star(f: Callable) -> Callable:
    """ Converts a function of multiple arguments, to a function taking a tuple.
        Useful for e.g. min(list_of_tuples, key=star(lambda a,b: ....)). """
    return lambda args: f(*args)


class Assert:
    def __init__(self, cond, opt_msg=None):
        assert cond, opt_msg

    @staticmethod
    def then_return(retval):
        return retval


def all_distinct(lst):
    return len(set(lst)) == len(lst)


def uniquify(lst: List[T], key: Optional[Callable[[T], K]] = None) -> List[T]:
    """Remove duplicates in lst but preserve the order

    Args:
        lst: a sequence with possible duplicates
        key: a mapping from an element of lst to its identity (default lambda x: x)
    """
    seen: Set[K] = set()
    result: List[T] = []
    for item in lst:
        k = key(item) if key is not None else item
        if k not in seen:
            # mypy issue: https://github.com/python/mypy/issues/3737
            seen.add(k)  # type: ignore
            result.append(item)
    return result


def read_file(file_name):
    with open(file_name) as f:
        return f.read()


def counter_to_list(counts):
    """ Takes a dict of form {elem: repeat_count} and constructs a list
        containing repeat_count occurrences of elem converted to float, for all items in the input """
    return [
        v
        for (elem, repeat_count) in counts.items()
        for v in [float(elem)] * repeat_count
    ]


def get_func_body(func_name, func):
    while func.op == "let":
        if func.first.name == func_name:
            return func.second
        func = func.third


def zip_values(d1: Dict[K, S], d2: Dict[K, T]) -> Dict[K, Tuple[S, T]]:
    """Return a dict with keys common to both,
        and values being tuples of the values in each dict. """
    return {k: (d1[k], d2[k]) for k in d1.keys() & d2.keys()}


class FunctionArgs:
    """Represents exactly what can be passed to a call of a Python function,
       that is a collection of positional arguments and keyword arguments."""

    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs

    def under(self, f):
        return f(*self.args, **self.kwargs)


def apply_args(f, args):
    return args.under(f)


def format_expr(expr_str, expr_limit):
    if len(expr_str) <= expr_limit:
        return expr_str
    # First remove brackets...
    e = expr_str.replace(")", "").replace("(", "")
    if len(e) <= expr_limit - 2:
        e = "({})".format(e)
    else:
        # then, shorten, being sure to insert ... to indicate it's been mangled
        l = (expr_limit - 5) // 2
        e = "({}...{})".format(e[:l], e[-l:])
    print("Expression {} too long, truncating to {}".format(expr_str, e))
    return e


def round_to_tol(num, rtol=1e-9):
    return round(num / rtol) * rtol


def singleton(cls):
    """Make a class a Singleton class (only one instance). Adapted from
    https://realpython.com/primer-on-python-decorators/#creating-singletons
    """

    @wraps(cls)
    def wrapper_singleton():
        if not wrapper_singleton.instance:
            wrapper_singleton.instance = cls()
        return wrapper_singleton.instance

    wrapper_singleton.instance = cls()
    return wrapper_singleton  # The value that goes into globals() for the module
    # For a singleton class Foo if we write Foo() we get the unique instance.


# NamedTuple does not allow overriding __new__ or __init__ in a subclass, only a subsubclass.
# So define fields here first. (See: https://github.com/python/typing/issues/526)
class _BestEpisodeEntry(NamedTuple):
    rules_name: str
    func_name: str
    rlo_or_expert: str  # May also be user
    config_or_git_id: str
    seq: List[Tuple[int, str]]
    raw_line: str


class BestEpisodeEntry(_BestEpisodeEntry):
    def __new__(
        cls, rules_name, func_name, rlo_or_expert, config_or_git_id, seq, raw_line
    ):
        assert rlo_or_expert in ["rlo", "expert", "user"]
        assert (len(config_or_git_id) > 0) or (rlo_or_expert == "user")
        return super().__new__(
            cls, rules_name, func_name, rlo_or_expert, config_or_git_id, seq, raw_line
        )


def parse_best_episodes(file_path: str) -> Iterable[BestEpisodeEntry]:
    """ Function to process lines from .kso file_path file,
        which starts with ; BEST, in order to extract
        'BestEpisodeEntry's. """
    for ln, raw_line in enumerate(read_file(file_path).split("\n")):
        if not raw_line.startswith("; BEST"):
            continue
        line = raw_line[6:]
        seq = json.loads(line[2 + line.find(";") :])
        values = [col for col in line[: line.find(";")].split(" ") if len(col) > 0]
        if len(values) == 3 and values[2] == "user":
            values.append("")
        elif len(values) != 4:
            raise ValueError(
                "Expected format\n"
                "; BEST <rules_name> <func_name> expert <git_hash>; <sequence in json> \n"
                "; BEST <rules_name> <func_name> rlo <config_run_id_name> ; <sequence in json> \n"
                "; BEST <rules_name> <func_name> user [<optional note_no_spaces>]; <sequence in json> \n"
                "but got {} in {}:{}".format(line, file_path, ln + 1)
            )
        yield BestEpisodeEntry(*values, seq, raw_line)  # type: ignore[call-arg]


# Taken from https://bugs.python.org/issue19495
@contextmanager
def elapsed_time(cb: Callable[[float, float], None], timer=time.perf_counter):
    t0 = timer()
    try:
        yield
    finally:
        t1 = timer()
        cb(t0, t1)


def permutation(nprng: np.random.Generator, seq: Sequence):
    """ Returns a copy of randomly permuted sequence
    """
    indices = nprng.permutation(len(seq))
    permuted = [seq[i] for i in indices]
    if isinstance(seq, tuple):
        return tuple(permuted)
    return permuted
