import pytest

from ksc.parse_ks import parse_ks_filename
from ksc.type_propagate import type_propagate_decls


def pytest_addoption(parser):
    parser.addoption("--backend", action="store", default="jax")


@pytest.fixture
def prelude_symtab():
    symtab = {}
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    return symtab
