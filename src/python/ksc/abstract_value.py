from collections import defaultdict
from ksc.utils import ShapeType, shape_type_from_object

def _cleanup_value(data):
    _, type = shape_type_from_object(data)
    if type.kind == "Tuple":
        return tuple(_cleanup_value(v) for v in data)
    if type.kind == "Vec":
        return None
    return data

class AbstractValue:
    def __init__(self, shape=None, type=None, data=None, context=None):
        # handle scalar shape output by shape$ function
        self._shape = () if shape == 0 else shape
        self._type = type
        self._data = data
        self._context = context

    @staticmethod
    def from_data(data, context=None):
        shape, type = shape_type_from_object(data)
        data = _cleanup_value(data)
        return AbstractValue(shape, type, data, context)

    @staticmethod
    def in_context(value, context):
        if not isinstance(value, AbstractValue):
            return AbstractValue.from_data(value, context)
        shape, type = value.shape_type
        return AbstractValue(shape, type, value.data, context)

    @staticmethod
    def abstract_like(value):
        if not isinstance(value, AbstractValue):
            shape, type = shape_type_from_object(value)
            return AbstractValue(shape, type)
        shape, type = value.shape_type
        return AbstractValue(shape, type, context=value.context)

    @property
    def shape_type(self):
        return ShapeType(self._shape, self._type)

    @property
    def data(self):
        return self._data

    @property
    def context(self):
        return self._context

    def __repr__(self):
        return f"AbstractValue(shape={self._shape}, type={self._type}, data={self._data}, context={self._context})"

def get_default_cost_config():
    return {
        "let_cost": 0.1,
        "assumed_vector_size": 100,
        "build_malloc_cost": 100,
        "index_cost": 1,
        "size_cost": 1,
        "select_cost": 0,
        "if_selection_cost": 1,
        "if_epsilon": 0.0001
    }

class ExecutionContext:
    """
    a context manager object that stores the costs under multiple states of execution.
    The current execution context is the last element of the global variable
    _execution_contexts
    """
    def __init__(self, config=None):
        self._costs = defaultdict(float)
        self._config = get_default_cost_config() if config is None else config

    @property
    def costs(self):
        return self._costs

    @property
    def config(self):
        return self._config

    def __enter__(self):
        global _execution_contexts
        _execution_contexts.append(self)
        return self

    def __exit__(self, type, value, traceback):
        global _execution_contexts
        _execution_contexts.pop()

    def accumulate_cost(self, name, state, cost):
        self.costs[state] += cost
        print(f"{self.__repr__()}: Added cost {cost:.2e} for {name} in state {state} (accumulated: {self.costs})")

_execution_contexts = []

def current_execution_context():
    if len(_execution_contexts) == 0:
        raise ValueError("No ExecutionContext found!")
    return _execution_contexts[-1]
