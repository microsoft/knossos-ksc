# import pytest

# from typing import List

from ksc.utils import ndgrid_inds
import itertools

def test_ndgrid_inds():

    val = list(ndgrid_inds((2,3)))
    assert val == [(i,j) for i in range(2) for j in range(3)]

    val = ndgrid_inds((2,3,4))
    assert list(val) == [(i,j,k) for i in range(2) for j in range(3) for k in range(4)]

