import pytest
import re

from ksc.expr import pystr
from ksc.parse_ks import parse_ks_filename


def test_syntax_primer():
    tlds = parse_ks_filename("test/ksc/syntax-primer.ks")
    for x in tlds:
        print(pystr(x, 0))
