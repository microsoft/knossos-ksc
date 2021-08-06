# fmt: off
from typing import Dict, Tuple, Optional

import numpy as np


# Store actual functions in a dictionary and return objects containing only name, to allow pickling.
_normalizers: Dict[str, Tuple] = {}

def available_cost_normalizers():
    return _normalizers.keys()

def cost_normalizer(name: Optional[str]):
    wrapper, _, _ = _normalizers["none" if name is None else name] # Default
    return wrapper

class CostNormalizer:
    """ A picklable container for a pair of functions that convert between two different scales

    Args:
        name: name of the normalizer
        norm: a function that takes a scalar unnormalized value and a scalar reference value
            and produces a normalized value. The reference value can be e.g., the starting cost
            of the expression.
        denorm: a function that takes an array of normalized values and an array of reference
            values and produces an array of values in the original scale

        We require that denorm([norm(v, r)], [r]) == [v], where v is an unnormalized value and
        r is a reference value

    """
    def __init__(self, name, norm, denorm):
        assert name not in _normalizers
        self.name = name
        _normalizers[name] = (self, norm, denorm)

    def __repr__(self):
        return "CostNormalizer({})".format(self.name)

    def normalize_value(self, value, ref_value):
        """ Produces a normalized value

        Args:
            value: scalar value to be normalized
            ref_value: scalar reference value

        Returns:
            a scalar normalized value
        """
        _, norm, _ = _normalizers[self.name]
        return norm(value, ref_value)

    def denormalize_values(self, values, ref_values):
        """ Produces an array of denormalized values

        Args:
            values: an array of normalized values
            ref_values: an array of reference values (the same values used for normalizing)

        Returns:
            an array of values in the original scale
        """
        _, _, denorm = _normalizers[self.name]
        return denorm(values, ref_values)

eps = 0.001

CostNormalizer("none",
    (lambda adv, _cost: adv),
    (lambda vals, _costs: vals)),

CostNormalizer("log",
    (lambda adv, _cost: np.log(adv+eps)),
    (lambda vals, _costs: np.exp(vals) - eps))

CostNormalizer("prop",
    (lambda adv, cost: adv/cost),
    (lambda vals, costs: vals * np.array(costs)[:,np.newaxis]))

CostNormalizer("logspeedup",
    (lambda adv, cost: -np.log(1-((adv - eps)/cost))),
    (lambda vals, costs: (1 - np.exp(-vals)) * np.array(costs)[:,np.newaxis] + eps))
