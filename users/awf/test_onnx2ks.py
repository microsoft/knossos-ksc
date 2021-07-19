import onnx

from ksc.parse_ks import parse_ks_file
from ksc.typeannot import typeannot_decls
import onnx2ks


def run_test_onnx2ks(filename):
    print(f"test_onnx2ks: Reading from {filename}")
    model = onnx.load(filename)

    # Convert to untyped KS
    decls = onnx2ks.onnx2ks(model.graph)

    # Check model
    onnx.checker.check_model(model)

    # Load preludes
    print(f"onnx2ks: Reading prelude")
    symtab = dict()

    prelude_decls = parse_ks_file("etc/onnx-prelude.ks")
    typeannot_decls(prelude_decls, symtab)

    prelude_decls = parse_ks_file("etc/onnx-prelude-autogen.ks")
    typeannot_decls(prelude_decls, symtab)

    # Apply type annotations
    typeannot_decls(decls, symtab)
    assert True


def test_minst():
    run_test_onnx2ks("test/onnx2ks/mnist-7.onnx")
