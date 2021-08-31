# Knossos IR

Knossos is a project that aims to accelerate writing AI algorithms to compiler
problems. There are different sub-projects in different languages, but all
using the same IR. This document aims to document the informal syntax of that IR.

#### The need for an informal syntax and semantics

Creating a new language and writing a compiler are both perilous tasks, but
doing both at the same time creates its own set of problems. Having multiple
compilers for the same new language, where each evolves separately brings a
uniquely unstable scenario.

Fixing the syntax is the first step. Allows all compilers to know they're parsing
valid code and exchange IR amongst themselves. But it does not guarantee that
the semantics are equivalent.

The raw syntax can be seen in action [here](test/ksc/syntax-primer.ks). That
file is used to make sure parsers can read the common parts of the Knossos IR
but it does not describe a self-contained semantics over all possible syntax
variations. That is the aim of this document.

## Basic Syntax

The basic building block of the Knossos syntax is the S-expression (also
used in Lisp and Scheme). That means that language constructs are identified by
nested parentheses. All white-space characters (space, line break) are ignored.

The constructs can be either a value or a list of values. Values have types
associated, inferred by either: literal initialisation, previous declaration
or argument/operand inference.

For example, `10.0` is a literal, parsed as `Float`, while in `(add x y)`, both
`x` and `y` are inferred to have the same type.

List of values are the basic block of the language, and their shapes determine
whether they are calls, conditionals, declarations, definitions, etc.

For example, `(let (x 10) x)` declares a variable named `x` and returns an
`Integer` of value `10`, while `(let (x (lam...)) (x y))` _calls_ a `Lambda`
called `x` using `y` as an argument.

### Comments

Single-line comments start with a semi-colon `;` and run to the end of the line.

Multi-line comments start with a `#|` and end with the reverse, `|#`.

Multi-line comments can be nested, so you need to close as many tokens as
you have opened.

## Types

Knossos is statically type-safe. Types that cannot be inferred statically
generate a compiler error.

Types are either declared explicitly (in function declarations/definitions) or
they're implied from the type of the arguments/operands.

The main classes of types are: Scalar, Composite and Callable types.

_Note: There are internal compiler types, such as the linear map, which are not
discussed here._

Type declarations are only used when declaring functions (_see `(edef)`_), which
are used for type validation.

### Scalar Types

#### String

`String` types are used mostly for debugging purposes. There are no standard
operations with strings available (such as concat, substr, etc).

`String` literals are values encompassed by quotes, for example: `"Hello world"`.

The only operation that works with strings is print (`(print)`).

Strings are also used in `(rule)` names, as to not pollute the symbol space
(variables and functions).

Support for extended character sets (like UTF-8) will depend on how the print
function is implemented by the compiler and is not guaranteed.

#### Bool

`Bool` types are binary, mainly used for conditionals (if/else), but as a
native scalar type, they can be composed with other types as well. They behave
as if they only have one bit, regardless of storage choices.

`Bool` literals are either `true` or `false`, case sensitive. Any variations will
be interpreted as a variable name.

#### Integer

`Integer` types are signed 64-bit types, regardless of the underlying
architecture.

`Integer` literals are numeric values without a decimal point, with optional
sign, for example: `42` and `-123`.

#### Float

`Float` types are 32-bit IEE 754 single-precision floating point numbers. Other
precisions are an open issue (https://github.com/microsoft/knossos-ksc/issues/358)

`Float` literals are numeric values with a decimal point, with optional sign,
for example: `10.0` and `-1.23`. Scientific notation is not supported.

##### to_float

The `to_float` construct converts an `Integer` into a `Float`. Ex:

```
; Declares an Integer variable and return as Float
(let (x 10) (to_float x))
```

### Composite Types

Composite types encompass other types in different ways. There are no composite
type literals, they must be created using specific Knossos constructs.

#### Tensor

`Tensor N` is a tensor of elements with the same type. Tensors are composable, so 
can contain any type, e.g. Tensor 2 (Tuple Float String (Tensor 3 Float))

The type declaration is simply: `(Tensor N Type)`. 

Tensors have no predetermined shape, nor a way to guarantee it at runtime. 
A tensor of tensors will be jagged (aka ragged) by default: each of the
inner Tensors could have a different size. 

##### build

Tensors need to be created algorithmically, either as the result of a
function or using the `build` construct.  Build takes a size and a lambda, e.g.
```scheme
; Creating a 7-element vector
(build 7 (lambda (i : Integer) (f i)))
; Creating a 5x7 matrix 
(build (tuple 7 5) (lambda (ij : Tuple Integer Integer) 
  (let ((i j) ij)
    (atan2 i j))))
; Creating a 5x7 (ragged) matrix of i-element vectors of constants
(build (tuple 7 5) (lambda (ij : Tuple Integer Integer) 
  (let ((i j) ij)
    (constVec i (to_float j)))))
```
Each element of _induction variable_ passed to the lambda will have the range `[0,N[`, ie.
zero inclusive, N exclusive.


##### size

Determining the runtime size of a tensor requires the `(size t)` construct.
It returns the size of the array as a tuple of Integers.

```scheme
; Returns the size of the second row of a (Tensor 1 (Tensor 1 Float))
(size (index 1 mat))
```

##### index

Accessing elements of a tensor requires the `(index (tuple N1 N2) t)` construct.
Each `N` has to be an `Integer`, zero-based.

```scheme
; Returning the _3rd_ element in a (Tensor 1 Type)
(index 2 v)
```

#### Tuple

`Tuple` is a list of elements of (not-necessarily) different shapes. Tuples are
composable, but unlike tensors, can nest at any element.

The type declaration is a list of types: `(Tuple Type1 Type2 ...)`. Similarly,
if the type is a tuple, it needs its own parenthesis: `(Tuple Type1 (Tuple
Type2 ...) ...)`.

Tuples are bound, and require the exact number of elements as the declaration.
There are no default assignment.

You can also have a tensor inside tuples: `(Tuple Float (Tensor 2 Integer) Bool)`.

##### tuple

`Tuple`s are constructed using the `(tuple ...)` construct. The
arguments' types define the tuple's arguments, which are checked when used.

```scheme
; Create a tuple of Integer, Float and Bool
(tuple 42 10.0 false)
```

##### get

Accessing elements of a tuple requires the `(get$i$N tup)` construct. `N` is the
number of elements of the tuple and `i` is an Integer between 1 and `N`, inclusive.

Unlike tensors, tuple _index_ starts at 1 and ends with N.

```scheme
; Returns the 7th element from a tuple of 9 elements
(let (7of9 (get$7$9 USSRaven)) 7of9)
```

### Callable

Callable objects implement the lambda-calculus and can be called like regular
functions. More importantly, they can define and compose behaviour directly
at the call site.

Like composite types, callable types don't have literals and need specific
Knossos constructs to be created.

#### Lambda

`Lam` is a functional type. It represents an object that can be called as
a function, and it's the required type of many other constructs (like `(build)`
and `(fold)`).

The type declaration has the argument types followed by the return type: `(Lam
 (Tuple Float Float) Float)`.

Lambdas are limited to a single argument, so tuples are used when more arguments
are needed.

_Note: Lambdas are not fully implemented in the language, as that would require
runtime memory management, function tables and argument marshalling. The current
compilers only implement in-place declarations inside the constructs defined
below, such as `(build)` and `(fold)`._

##### lam

`Lambda`s are created via the `(lam (arg) (expr))` construct. The argument is a
variable declaration of the form `(var : Type)` and the expression is any valid
Knossos expression.

```scheme
; A lambda that returns the sum of two numbers
(lam (t : (Tuple Float Float)) (add (get$1$2 t) (get$2$2 t)))
```

#### Linear Maps

Maps one type onto another, used in auto-generated functions and are not meant
for user consumption. They only show in function declarations (where the
definition is inside the compiler run-time library) and opaquely implemented.

Their syntax is: `(edef D$eq@ff (LM (Tuple Float Float) Bool) (Float Float))`,
which means the return type is a linear map that translates a tuple of two floats
into one boolean, given the two floats to compare. The `D$` prefix means this
is an auto-generated derivative of the function `eq` for two floats `@ff`.

## Basic Constructs

### Callable Names

In Knossos, operations, functions and lambdas are indiscernible. They all have
return types, arguments and a name. The main difference is that the operations
are declared by the compiler (in `src/runtime/prelude.ks`) and implemented by
the run-time (in Futhark, C++, etc.), while functions and lambdas are declared
in user code.

A call to a function, with syntax `(fname arg0 arg1 ...)` is only valid if it
has been declared (either by the compiler or user) and the arguments match in
number and types, as well as having the same return type.

_Note: Lambdas are not a language first-class citizen, so existing compilers
may not implement the full breath that is required to treat them like normal
functions. They should work in `build`, `fold`, etc. but not necessarily
when called directly._

### Operations

Operations are pre-declared by the compiler with the following naming scheme:
`name@types`. The types are `i` for integers and `f` for float and the names
separate into four categories:

 * **Arithmetic**: `add, sub, mul, div, neg`, for both `Float` and `Integer`.
 * **Comparison**: `eq, ne, gt, lt, gte, lte`, for both `Float` and `Integer`.
 * **Maths**: `log, exp, sin, cos, max, min, abs` for `Float`
 * **Type Conversion**: `to_float` for `Integer`

Compilers will also generate their auto derivative, appending `D$` to
their names, but those should be used by the compiler only during
auto-derivation.

Operations cannot be redefined by users, and on binary operations, both types
have to be the same. Return types are also the same, except on conversion
operations.

_Note: There are examples where `(add x y)` is used, which means some compilers
do try to assume the type of the operation by its arguments, but you should
not rely on that. Using `(add@ff x y)` is always safe._

### Functions

Functions are _user defined operations_, and as such, need to be declared and/or
defined before being used.

#### Declaration

Declarations describe the _signature_ of the function, ie. its name, return type
and argument types, but not its argument names (which are unnecessary).

The syntax is: `(edef name RetTy (ArgTy0 ArgTy1 ...))`. Any valid type can be
used for return and argument types.

Declared functions can be called but are not implemented, so unless it's
implemented somewhere else (compiler run-time libraries, another source file),
it will lead to a linking error if you try to execute.

```scheme
; Declares a function that takes in two tuples and add them element wise
(edef tadd@ff (Tuple Float Float) ((Tuple Float Float) (Tuple Float Float)))
```

#### Definition

Definitions show how the function is implemented, ie. define the function body
and name the arguments that are used within.

The function definition _signature_ must match its declaration, if any. If there
are no declarations with the same types, a new declaration is registered. This
means you can directly define functions without declaring them first, but be
careful, it also means if you make a mistake in the definition, the compiler will
assume you want a new function instead.

The syntax is: `(def name RetTy ((arg0 : ArgTy0)(arg1 : ArgTy1)...) (body))` with
`body` any valid expression.

The arguments are bound on call and can be used inside the function body.

```scheme
; Implements the tuple sum declared above
(def tadd (Tuple Float Float)
             (a : (Tuple Float Float) (b : (Tuple Float Float)))
             (tuple (add@ff (get$1$2 a) (get$1$2 b))
                    (add@ff (get$2$2 a) (get$2$2 b))
             )
)

; Directly defines a zip function, from two vectors, build a third vector that
; holds a tuple for every pair of elements ({a[0], b[0]}, ... {a[N-1], b[N-1]})
; Asserts (size a) == (size b).
(def zip  (Tensor 1 (Tuple Float Float))
             ((a : (Tensor 1 Float)) (b : (Tensor 1 Float)))
             (assert (eq (size a) (size b))
               (build (size a) (lam (i : Integer)
                               (tuple (index i a) (index i b)))
               )
             )
)
```

#### Call

Calling a function is just like an operation. The return type, arguments order
and types must match. However, unlike operations, the arguments and types are
defined by the declaration/definition.

The syntax is: `(fname arg0 arg1 ...)`

```scheme
; Recursive function that sums a vector of tuples, element-wise
(def tsum (Tuple Float Float)
          (
           (i : Integer)
           (v : (Tensor 1 (Tuple Float Float)))
           (acc : (Tuple Float Float))
          )
          ; One past last, tail return accumulated value
          (if (eq i (size v))
              acc
          ; Every other element, increment and tail recurse
          (tsum@ff (add@ii i 1) v (tadd@ff acc (index i v))))
)

; Wrapper that zips two lists and calls tsum@ff
(def tsum_list (Tuple Float Float) ((a : (Tensor 1 Float)) (b : (Tensor 1 Float)))
               (assert (eq (size a) (size b))
                       (tsum (0 (zip@ff a b) (tuple 0.0 0.0))))
)

; Print the reduced value
(def tsum_print () ((a : (Tensor 1 Float)) (b : (Tensor 1 Float)))
                (let (s (tsum_list a b))
                     (print
                        (get$1$2 s)
                        " "
                        (get$2$2 s)
                     )
                )
)
```

_Note: `tadd@ff` above is called with the accumulator and each element of the
vector `v`, both of which are `(Tuple Float Float)`, while `zip@ff` is called
with the two vectors `a` and `b` (and will assert if they're not the same),
returning a `(Tensor 1 (Tuple Float Float))`, as expected._

### Variables

Variables are declared in two ways: either as function arguments in definitions
or as `(let)` constructs. Both have limited lexical context and can be shadowed
(new variables with the same name).

Function arguments' context is the function body, while `(let)` constructs
define variables on an enclosed context, within their `(expr)` blocks.

In the function definitions above, the arguments declared (`a`, `b`) are only
in context inside the `(def)` block, and so do `s` and `v` declared by the
`(let)`, inside the `(expr)` block.

#### Let

Explicit variable definitions, with optional expression block where the variable
is valid.

The syntax is: `(let (name (initialiser)) (expr))`

The variable `name` is initialised to the expression `initialiser` and can be
used in the `(expr)` block.

```scheme
; `argc` and `argv` are valid throughout the body of `main`
(def main Integer ((argc : Integer) (argv : (Tensor 1 String)))
                  ; Declares `a`, `b` and `c`
                  (let ((a 10) (b 12.3) (c (index 1 argv)))
                    ; Uses `a`, redefines `b`
                    (let ((d (add@ii a 10)) (b (sub@ff (to_float d) b)))
                      ; returns new `b`
                      b
                    )
                  )
)
```

### Conditionals

Conditionals return one of two values, depending on the condition evaluated
as boolean. Each value is calculated from a generic expression and both have to
have the same return type. The condition can be any expression that evaluates to
`Bool`.

The syntax is: `(if (cond) (then expr) (else expr))`

```scheme
; Sum the two elements of a tuple
(def tsum@tff (Float) (t : (Tuple Float Float))
              (add@ff (get$1$2 t) (get$2$2 t)))

; Returns the max of the sum of two tuples
(def tmax@tfftff (Tuple Float Float)
                 ((a : (Tuple Float Float)) (b : (Tuple Float Float)))
                 (if (gt@ff (tsum@tff a) (tsum@tff b)) a b)
)
```

### Fold

Folds a vector using a lambda-defined reduction operation and returns the
accumulator. Lambdas _have_ to have a single argument, but fold requires two:
one for the accumulator and one for the vector element type, so lambdas inside
folds _have_ to use a tuple of two types.

Both accumulator and vector element type can be anything, so the tuple can
have any two types.

The syntax is: `(fold (lam (acc : (Tuple AccTy ElmTy)) (body) init vector))`

The body must return a value of the accumulator type `AccTy`. Each iteration
takes on the previous accumulator and passes is as the first element in the tuple
argument for the next call, with the next vector element.

In loop form, this would be equivalent to:
```
AccTy acc <- init
loop [0..(size vector)-1] -> i:
  acc = lam(tuple acc vec[i])
return acc
```

Example:
```scheme
; Returns the product of all elements of `v` (multiply-reduce)
(def prod_fold Float ((v : Tensor 1 Float))
     (fold (lam (acc_x : (Tuple Float Float))
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  (mul@ff acc x)))
           1.0
           v
     )
)
```

Any existing variable in context (environment) can be used, for example:

```scheme
; Returns prod(v) * pow(closure, size(v))
(def prod_fold_closure Float ((v : Tensor 1 Float) (closure : Float))
     (fold (lam (acc_x : Tuple Float Float)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  (mul@ff (mul@ff acc x) closure)))
           1.0
           v
     )
)
```

### Extra

Other functions used in this document.

#### assert

Checks for a run-time condition before executing some code. Stops execution
if condition doesn't hold. Returns the value and type returned by `(expr)`.

Syntax: `(assert (cond) (expr))`

#### print

Prints a list of values of numeric or string types, from literals, expressions, etc.

Returns the number of elements printed as Integer.

Syntax: `(print value expr literal ... )`
