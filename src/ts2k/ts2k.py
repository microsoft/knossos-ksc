import torch

import os
import importlib.util
import inspect
import argparse
from pathlib import Path
from ksc import utils

from ksc.torch_frontend import ts2ks, write_edefs

parser = argparse.ArgumentParser(description="Convert TorchScript to Knossos IR")
parser.add_argument("--input_file", required=True, help="TorchScript [file_path].py")
parser.add_argument("--output_file", required=True, help="Knossos IR [file_path].ks")
parser.add_argument(
    "--generate_edef",
    action="store_true",
    help="Generate edef, otherwise default to pass-through",
)
args = parser.parse_args()

input_file_path = args.input_file
output_file_path = args.output_file
generate_edef = args.generate_edef

input_directory = os.path.dirname(input_file_path)

output_directory = os.path.dirname(output_file_path)
if output_directory != "":
    os.makedirs(output_directory, exist_ok=True)
output = open(output_file_path, "w")


module_name = "DynamicLoadedModule"

with utils.add_to_path(input_directory):
    spec = importlib.util.spec_from_file_location(module_name, Path(input_file_path))
    dynamicModule = importlib.util.module_from_spec(spec)
    # We deliberately don't make visible via sys.modules[module_name] = module
    spec.loader.exec_module(dynamicModule)

# load all TorchScript methods in target modules
# (various limitations, local sibling modules)
dynamicMembers = inspect.getmembers(
    dynamicModule, predicate=lambda v: type(v) == torch.jit.ScriptFunction
)

output.write("#| -------- Graph ----------")
output.write("\n")
output.write("\n")

for (name, member) in dynamicMembers:
    output.write(str(member.graph))
    output.write("\n")
output.write("-------- |#")
output.write("\n")
output.write("\n")


if generate_edef:
    write_edefs(output)

for (name, member) in dynamicMembers:
    ts2ks(output, generate_edef, member)
    output.write("\n\n")
    # print(name)
