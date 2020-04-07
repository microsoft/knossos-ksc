//===- KnossosOps.td - Knossos dialect ops -----------*- tablegen -*-===//
//
// This file is licensed under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef KNOSSOS_OPS
#define KNOSSOS_OPS

include "KnossosDialect.td"
include "mlir/Interfaces/CallInterfaces.td"
include "mlir/Interfaces/SideEffects.td"

// The aim of the Knossos dialect is to transition the Knossos RLO engine from
// Knossos IR to MLIR. There should be no intrinsic reason why the dialect is
// necessary over existing ones, and the engine should be generic enough to be
// able to cope with any MLIR dialect that conforms to specific interfaces and
// traits.
//
// Scalar types like int, float, boolean, as well as arithmetic and comparison
// operations are used form the standard dialect. Here, we just declare the
// difference between Knossos and Standard, which are:
//
// Types:
//   Tuples can have any number of elements and types, including tuples
//   Arrays can only have one sub-type, which can be another array
//   Strings, which are mostly used for printing
//   Lambdas, which for now are only directly applied to build and fold
//
// Constructs:
//   (build) creates arrays from range and lambda
//     (index) access array elements, (size) returns their length
//     (fold) reduces an array to a scalar via a lambda and an initial value
//   (tuple) creates a tuple from dynamic arguments
//     (get) retrieves the nth element of a tuple
//   (let) defines scope for variables
//   (efed) & (def) declared and define functions
//   (if) selects between two expressions, given a condition
//   (lam) creates a lambda, so far used only in (build) and (fold)
//
// Standard Library:
//    sum, zip, dot, abs, neg, max, min, sin, cos, exp...
//    most of which have existing MLIR constructs

//===----------------------------------------------------------------------===//
// Types
//===----------------------------------------------------------------------===//

// Primitive type support:
//  - lambdas are represented by regions (static)
//  - only numeric types (no string), fixed width, bool is special
def BoolType : TypeAlias<I1>;
def DataType : AnyTypeOf<[I64, F64]>;
def ScalarType : AnyTypeOf<[DataType, BoolType]>;
//  - vectors are tensors and can only be int or float (multi-dim)
def VectorType : AnyTypeOf<[I64Tensor, F64Tensor]>;
//  - tuples can only hold numeric types or vectors
def TupleType : TupleOf<[ScalarType, VectorType]>;

def DataTypeAttr
    : Attr<CPred<"$_self.isa<DataTypeAttr>()">, "datatype attribute"> {
  let storageType = [{DataTypeAttr}];
  let returnType = [{Type}];
}

def LiteralAttr : Attr<CPred<"$_self.isa<LiteralAttr>()">, "literal"> {
  let storageType = [{LiteralAttr}];
  let returnType = [{std::string}];
}

//===----------------------------------------------------------------------===//
// Tuple constructs
//===----------------------------------------------------------------------===//

// %tup = tuple %a : i64, %b : f64, ... -> tuple<i64, f64, ...>
def TupleOp : Knossos_Op<"tuple", [NoSideEffect]> {
  let summary = "tuple creation";
  let description =
      "This operation returns a tuple of variadic number of elements.";
  let arguments = (ins Variadic<ScalarType> : $elms);
  let results = (outs TupleType);
  let assemblyFormat = "operands `:` functional-type(operands, results) attr-dict";
}

// %elm = get %i : i64, %tup : tuple<i64, f64, ...> -> Ty
// Ty must be the type in tuple<...> of the nth element (static/dynamic check)
def GetOp : Knossos_Op<"get", [NoSideEffect]> {
  let summary = "tuple access";
  let description =
      "This operation returns the value of the nth element of a tuple.";
  let arguments = (ins I64 : $idx, TupleType : $tup);
  let results = (outs ScalarType);
  let assemblyFormat = "operands `:` functional-type(operands, results) attr-dict";
}

//===----------------------------------------------------------------------===//
// Vector constructs
//===----------------------------------------------------------------------===//

// %vec = build %size : i64, @build0(%iv) : (i64) -> tensor<?xTy>
// Dimension ? fully specified by %size. Static for constants, dynamic for exprs
// FIXME: format is not right, work out how to define the function argument
def BuildOp : Knossos_Op<"build", [NoSideEffect]> {
  let summary = "vector builder";
  let description =
      "This operation returns a vector built by a lambda.";
  let arguments = (ins I64 : $size, FlatSymbolRefAttr:$callee);
  let results = (outs VectorType);
  let assemblyFormat = "$callee `(` $size `)` attr-dict `:` functional-type($size, results)";
}

// %elm = index %i : i64, %vec : tensor<?xTy> -> Ty
// assert(%i < %size) static for constants, dynamic otherwise
def IndexOp : Knossos_Op<"index", [NoSideEffect]> {
  let summary = "vector access";
  let description =
      "This operation returns the Nth element of a vector.";
  let arguments = (ins I64 : $idx, VectorType : $vec);
  let results = (outs DataType);
  let assemblyFormat = "operands `:` functional-type(operands, results) attr-dict";
}

// %len = size %vec : tensor<?xTy> -> i64
// No exception, static for constant, "dim tensor" otherwise
def SizeOp : Knossos_Op<"size", [NoSideEffect]> {
  let summary = "vector length";
  let description =
      "This operation returns the length of a vector.";
  let arguments = (ins VectorType : $vec);
  let results = (outs I64);
  let assemblyFormat = "operands `:` functional-type(operands, results) attr-dict";
}

// %sum = fold %vec : tensor<?xTy>, @init0() : () -> Ty, @fold0(%iv) : (Ty) -> RedTy
// Reduction type (RedTy) not necessarily the same as vector element type (Ty)
// FIXME: format is not right, work out how to define the function argument
def FoldOp : Knossos_Op<"fold", [NoSideEffect]> {
  let summary = "reduce construct";
  let description =
      "This operation returns a reduced value built by a vector and a lambda.";
  let arguments = (ins VectorType : $vec, FlatSymbolRefAttr:$init, FlatSymbolRefAttr:$red);
  let results = (outs DataType);
  let assemblyFormat = "$init `(` `)` `,` $red `(` $vec `)` attr-dict `:` functional-type($vec, results)";
}

//===----------------------------------------------------------------------===//
// Scope constructs
//===----------------------------------------------------------------------===//

// Functions are declared and defined via standard MLIR

// %ret = let { %x = init : Ty0; ret expr(%x) : Ty1 } -> Ty1
def LetOp : Knossos_Op<"let", [NoSideEffect]> {
  let summary = "lexical context";
  let description =
      "This operation encloses a variable inside a lexical context.";
  let arguments = (ins ScalarType : $expr);
  let results = (outs ScalarType);
  let assemblyFormat = [{ $expr `:` type($expr) attr-dict `:` type(results) }];
}

// %ret = if @cond0() : () -> i1, @true0() : () -> Ty, @false0() : () -> Ty
// FIXME: Should we do this as basic blocks (and use successors)?
def IfOp : Knossos_Op<"if", [NoSideEffect]> {
  let summary = "select operation";
  let description =
      "This operation selects between two expressions based on a boolean condition.";
  let arguments = (ins FlatSymbolRefAttr:$c, FlatSymbolRefAttr:$t, FlatSymbolRefAttr:$f);
  let results = (outs ScalarType);
  let assemblyFormat = "$c `(` `)` `,` $t `(` `)` `,` $f `(` `)` attr-dict `:` functional-type(operands, results)";
}

//===----------------------------------------------------------------------===//
// Operations
//===----------------------------------------------------------------------===//

// In Knossos, all operations are calls to functions, and calls are standard MLIR

// A lot of knossos operations can be represented in MLIR directly and
// efficiently (add, sub..., exp, log...), and those are polymorphic in knossos.

// We should be able to differentiate them here and lower directly to instructions
// instead of calls, but this is a front-end decision, not a dialect one.

#endif // KNOSSOS_OPS
