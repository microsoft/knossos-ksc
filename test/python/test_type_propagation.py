
from ksc.type_propagate import type_propagate_decls
from ksc.parse_ks import parse_ks_filename

def test_type_propagate():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    decls_file = list(parse_ks_filename("test/ksc/syntax-primer.ks"))
    list(type_propagate_decls(decls_file, symtab))
