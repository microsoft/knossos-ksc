
from ksc.typeannot import typeannot_decls
from ksc.parse_ks import parse_ks_filename

def test_typeannot():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    typeannot_decls(decls_prelude, symtab)
    decls_file = list(parse_ks_filename("test/ksc/syntax-primer.ks"))
    list(typeannot_decls(decls_file, symtab))
