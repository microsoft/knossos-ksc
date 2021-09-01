"""
Correctness test for blas_combined.kso

Requires:
 - pytest
 - scipy
 - ksc
"""
# fmt: off
import pytest
import numpy as np
import scipy.linalg

from rlo import utils
from ksc.utils import translate_and_import  # pylint:disable=no-name-in-module

def make_random_normal(n, v, sz):
    """ Similar to np.random.normal, but makes a list of (lists of...) 1d numpy arrays,
        rather than a 2d/3d/etc. numpy array. This is so that the python-ksc interpreter
        treats these as Vec (Vec Float), where size(v) is an int,
        rather than Tensor 2 Float, where size(v) is a (Tuple int int). """
    if len(sz)==1:
        return np.random.normal(n, v, sz)
    else:
        return [make_random_normal(n, v, sz[1:]) for _ in range(sz[0])]

# pylint:disable=redefined-outer-name
@pytest.fixture
def blas_module():
    ks_str = utils.read_file("src/rlo/ksc/blas/blas_combined.kso")
    return translate_and_import(__file__, ks_str, "jax")

def compute_blas(module, func_name, **kwargs):
    output = np.array(module.__dict__[func_name](**kwargs))
    expected_output = scipy.linalg.blas.__dict__["d" + func_name](**kwargs)
    return output, expected_output

def check_blas_upper(module, func_name, **kwargs):
    output, expected_output = compute_blas(module, func_name, **kwargs)
    rows, cols = output.shape  # pylint:disable=unpacking-non-sequence
    iu = np.triu_indices(n=rows, m=cols)
    assert np.allclose(expected_output[iu], output[iu])

def check_blas(module, func_name, **kwargs):
    output, expected_output = compute_blas(module, func_name, **kwargs)
    assert np.allclose(expected_output, output)

def test_axpy(blas_module):
    x = make_random_normal(0, 1, (32,))
    y = make_random_normal(0, 1, (32,))
    a = 2.23
    check_blas(blas_module, "axpy", x=x, y=y, a=a)

def test_add_index_scal(blas_module):
    a = 1.234
    x = make_random_normal(0, 1, (32,))
    y = make_random_normal(0, 1, (32,))
    output = blas_module.add_index_scal(a, x, y)
    expected_output = a * x + y
    assert np.allclose(expected_output, output)

def test_scal(blas_module):
    x = make_random_normal(0, 1, (32,))
    a = 1.234
    check_blas(blas_module, "scal", x=x, a=a)

def test_dot(blas_module):
    x = make_random_normal(0, 1, (32,))
    y = make_random_normal(0, 1, (32,))
    check_blas(blas_module, "dot", x=x, y=y)

def test_rot(blas_module):
    x = make_random_normal(0, 1, (32,))
    y = make_random_normal(0, 1, (32,))
    c = np.cos(0.5)
    s = np.sin(0.5)
    check_blas(blas_module, "rot", x=x, y=y, c=c, s=s)

def test_nrm2(blas_module):
    x = make_random_normal(0, 1, (32,))
    check_blas(blas_module, "nrm2", x=x)

def test_asum(blas_module):
    x = make_random_normal(0, 1, (32,))
    check_blas(blas_module, "asum", x=x)

def test_gemv(blas_module):
    alpha = 1.234
    a = make_random_normal(0, 1, (3, 5))
    x = make_random_normal(0, 1, (5,))
    beta = 2.23
    y = make_random_normal(0, 1, (3,))
    check_blas(blas_module, "gemv", alpha=alpha, a=a, x=x, beta=beta, y=y)

def test_trmv(blas_module):
    a = make_random_normal(0, 1, (10, 10))
    x = make_random_normal(0, 1, (10,))
    check_blas(blas_module, "trmv", a=a, x=x)

def test_ger(blas_module):
    alpha = 1.234
    x = make_random_normal(0, 1, (5,))
    y = make_random_normal(0, 1, (7,))
    a = make_random_normal(0, 1, (5, 7))
    check_blas(blas_module, "ger", alpha=alpha, x=x, y=y, a=a)

def test_syr2(blas_module):
    alpha = 1.234
    x = make_random_normal(0, 1, (5,))
    y = make_random_normal(0, 1, (5,))
    a0 = np.random.normal(0, 1, (5, 5))
    a = (a0 + a0.T) / 2
    check_blas_upper(blas_module, "syr2", alpha=alpha, x=x, y=y, a=a)

def test_gemm(blas_module):
    alpha = 1.234
    a = make_random_normal(0, 1, (5, 7))
    b = make_random_normal(0, 1, (7, 11))
    beta = 2.23
    c = make_random_normal(0, 1, (5, 11))
    check_blas(blas_module, "gemm", alpha=alpha, a=a, b=b, beta=beta, c=c)

def test_amxmy(blas_module):
    alpha = 1.234
    mx = make_random_normal(0, 1, (7, 11))
    my = make_random_normal(0, 1, (11, 13))
    output = blas_module.amxmy(alpha, mx, my)
    expected_output = alpha * np.dot(mx, my)
    assert np.allclose(expected_output, output)

def test_apbvx(blas_module):
    a = 1.234
    b = 2.23
    x = make_random_normal(0, 1, (32,))
    output = blas_module.apbvx(a, b, x)
    expected_output = a + b * x
    assert np.allclose(expected_output, output)

def test_avxpavy(blas_module):
    a = 1.234
    x = make_random_normal(0, 1, (32,))
    y = make_random_normal(0, 1, (32,))
    output = blas_module.avxpavy(a, x, y)
    expected_output = a * x + a * y
    assert np.allclose(expected_output, output)

def test_avxvxpb(blas_module):
    a = 1.234
    x = make_random_normal(0, 1, (32,))
    b = 2.23
    output = blas_module.avxvxpb(a, x, b)
    expected_output = a * (x * x) + b
    assert np.allclose(expected_output, output)

def test_amxpmy(blas_module):
    a = 1.234
    x = make_random_normal(0, 1, (7, 11))
    y = make_random_normal(0, 1, (7, 11))
    output = blas_module.amxpmy(a, x, y)
    expected_output = a * np.array(x) + np.array(y)
    assert np.allclose(expected_output, output)

def test_amxpamy(blas_module):
    a = 1.234
    x = make_random_normal(0, 1, (7, 11))
    y = make_random_normal(0, 1, (7, 11))
    output = blas_module.amxpamy(a, x, y)
    expected_output = a * np.array(x) + a * np.array(y)
    assert np.allclose(expected_output, output)

def test_amxmxpb(blas_module):
    a = 1.234
    x = make_random_normal(0, 1, (7, 11))
    b = 2.23
    output = blas_module.amxmxpb(a, x, b)
    x_arr = np.array(x)
    expected_output = a * (x_arr * x_arr) + b
    assert np.allclose(expected_output, output)
