# Knossos MLIR libraries

This tree aims to implement two libraries: a Knossos file reader that emits MLIR,
and an API providing introspection into an MLIR graph for heuristics search.

## MLIR back-end

The MLIR back-end parsers Knossos IR and emits MLIR using as much as possible of
the MLIR core/affine/linalg dialects.

This back-end prints out an MLIR graph (text, binary) when requested, or it
feeds into the introspection API library for further processing.

The library should also be able to import MLIR graphs, for round-trip testing
and interacting with other MLIR tools.

**Partial implementation. See TODO below.**

## MLIR Heuristics Introspection Engine

Provides a stable way to query MLIR graphs for the purpose of heuristics
searches. We may need a Knossos-specific dialect.

If we do, we'll need to be able to annotate generic MLIR graph with Knossos
dialect, as well as read Knossos IR into MLIR into that dialect.

**Not implemented yet.**

# Knossos MLIR Dialect

This is a stub for the Knossos [MLIR](https://mlir.llvm.org/) dialect along with a tool to operate on that dialect.

We may in the future decide to use the [Rise](https://github.com/rise-lang/mlir) dialect. But the decision should be orthogonal with the decision to use the [Elevate](https://arxiv.org/pdf/2002.02268.pdf) rewrite description on either dialect.

## Building Ksc-MLIR

This setup assumes that you have [built](https://mlir.llvm.org/getting_started/) LLVM and MLIR in `$BUILD_DIR` and installed them to `$PREFIX`. To build and launch the tests, run

```sh
# Prepare
$ export LLVM=/path/to/llvm-project/build
$ cd path/to/knossos-ksc/mlir
$ mkdir build && cd build

# Build
$ cmake -G Ninja .. -DMLIR_DIR=$LLVM/lib/cmake/mlir -DLLVM_EXTERNAL_LIT=$LLVM/bin/llvm-lit
$ ninja ksc-mlir

# Test
$ ninja check-ksc

# Use
$ ./bin/ksc-mlir TEST -vvv # Runs the unit tests
$ ./bin/ksc-mlir AST foo.ks # Spits out AST
$ ./bin/ksc-mlir MLIR foo.ks # Spits out MLIR
$ ./bin/ksc-mlir LLVM foo.ks # Spits out LLVM IR
```

**Tested with:**
 * Compilers: Clang 6 and 9, GCC 7.4 and 9.2
 * Linkers: GNU and LLD
 * OS: Ubuntu 18 and 19

Should work with any compiler, linker and OS LLVM works with.

## Building LLVM/MLIR

```bash
# Prepare
$ apt install clang ninja-build cmake lld
$ git clone git@github.com:llvm/llvm-project.git
$ cd llvm-project
$ mkdir build && cd build

# Build
$ cmake -G Ninja ../llvm \
        -DLLVM_ENABLE_PROJECTS=mlir \
        -DLLVM_BUILD_EXAMPLES=ON \
        -DLLVM_TARGETS_TO_BUILD="X86" \
        -DCMAKE_BUILD_TYPE=Debug \
        -DLLVM_ENABLE_ASSERTIONS=ON \
        -DCMAKE_C_COMPILER=clang \
        -DCMAKE_CXX_COMPILER=clang++ \
        -DLLVM_ENABLE_LLD=ON
$ ninja check-mlir
```

# Implementation Details

The Lexer/Tokeniser creates two different kinds of tokens: values and non-values.
This is consistent with existing continuation compilers.

The expected sequence of value and non-value tokens and the token's value will
determine which kind of AST node we're parsing.

The Parser reads the Token tree and creates the AST, which is then used by the
MLIR back end to generate a module. The module can be optimised and LLVM IR
generated from it.

In the future, the heuristics engine will use the MLIR module to optimise the
graph, and there will be the option of either running the result in a JIT or
lowering to specific hardware.

**Back-end TODO:**
 * Add support for local variables (context) & finish Knossos IR support.
 * Run through multiple Ksc examples, add FileCheck validation.

**Engine TODO:**
 * Cost Model: liveness analysis, target-specific hooks, PGO, etc.
 * Navigation: find users/ops, dominance analysis, control flow, patterns, etc.
 * Rewrite dialect: known transformations representation, heuristics costs.
 * Heuristics: query/add/change heuristics values based on cost and predictions.
 * Traversal: allows sub-trees to be selected, pruned, cloned, modified, etc.
 * Lowering: either to a JIT execution engine or to specific hardware languages.
