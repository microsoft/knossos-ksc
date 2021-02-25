import pytest

from ksc.type import Type
from ksc.expr import StructuredName, make_structured_name

from ksc.parse_ks import s_exps_from_string, parse_structured_name

def parse(s : str) -> StructuredName:
  ses = s_exps_from_string(s)
  assert len(ses) == 1
  return parse_structured_name(ses[0])

def test_StructuredName():
  sn = make_structured_name(("rev", ("f", Type.Integer)))
  assert sn.mangled() == "rev$f@i"

  assert parse("[rev [f Integer]]") == sn

def test_StructuredName_manglers():
  assert parse("[rev foo]").mangle_without_type() == "rev$foo"
  assert parse("[foo (Tuple Float Float)]").mangle_without_type() == "foo"
  assert parse("[rev [fwd [foo (Tuple Float Float)]]]").mangle_without_type() == "rev$fwd$foo"

  assert parse("[rev foo]").mangled() == "rev$foo"
  assert parse("[foo (Tuple Float Float)]").mangled() == "foo@<ff>"
  assert parse("[rev [fwd [foo (Tuple Float Float)]]]").mangled() == "rev$fwd$foo@<ff>"
