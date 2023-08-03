# Invoke ksc on a file, treating any def which appears in the file as a root.
# (But any unused functions in the prelude are pruned.)

import subprocess
import sys

from ksc import utils
from ksc.parse_ks import parse_ks_filename
from ksc.expr import Def, GDef
from ksc.type_propagate import type_propagate_decls


def function_name(decl):
    if isinstance(decl, Def):
        return str(decl.name)
    # elif isinstance(decl, GDef):
    #     return str(decl.name())
    else:
        return None


def prune_and_compile(source_file, output_file):
    ksc_path, ksc_runtime_dir = utils.get_ksc_paths()

    symtab = dict()
    decls_prelude = list(parse_ks_filename(f"{ksc_runtime_dir}/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    decls = list(parse_ks_filename(source_file))
    type_propagate_decls(decls, symtab)

    function_names = [
        fn for fn in (function_name(decl) for decl in decls) if fn is not None
    ]

    ksc_command = [
        ksc_path,
        "--generate-cpp",
        "--ks-source-file",
        f"{ksc_runtime_dir}/prelude.ks",
        "--ks-source-file",
        source_file,
        "--ks-output-file",
        output_file,
        "--cpp-include",
        "prelude.h",
        "--cpp-output-file",
        "/dev/null",
        "--remove-unused",
        *(opt for function_name in function_names for opt in ("--used", function_name)),
    ]

    try:
        e = subprocess.run(ksc_command, capture_output=True, check=True)
    except subprocess.CalledProcessError as e:
        ksc_command_str = " ".join(ksc_command)
        print(f"Command failed:\n{ksc_command_str}")
        print("KSC output:\n")
        print(e.output.decode("ascii"))
        ksc_stderr = e.stderr.decode("ascii")
        ksc_stderr_filtered = "> " + "\n> ".join(ksc_stderr.split("\n")[:-1])
        print(ksc_stderr_filtered + "\n")
        raise Exception(
            f"Command failed:\n"
            f"{ksc_command_str}\n"
            f"KSC output:\n"
            f"{ksc_stderr_filtered}\n"
        ) from e


if __name__ == "__main__":
    assert len(sys.argv) == 3
    prune_and_compile(sys.argv[1], sys.argv[2])
