# Knossos-KSC  

#### Compile a lisp-like IR with automatic differentiation and user-defined rewrites.

This project is a functional compiler and code-gen tool that will
accelerate writing AI algorithms as well as making them easier.   The core is a lisp-like IR that can be translated from high-level languages, and can be linked to a variety of backends to generate code.

Currently implemented frontends are
 * TorchScript: ts2k
 * ONNX: onnx2ks
 * Julia: j2ks
 * F#: f2k
 * KS-Lisp: The IR itself is exchanged in a lisp-like text format (see below).  

Current backends:
 * CPU/C++: In src/ksc and src/runtime
 * GPU/Futhark: Also in src/ksc
 * MLIR: In [mlir](mlir/README.md)

Current transformers:
 * KSC: Various Autodiff and optimization transforms, in Haskell 

### KS-Lisp: A low-sugar IR

The text form of the IR is "low sugar", focused more on simplicity than user-friendliness.
However it *is* human writable, and lispers in particular may like to play with it.
There's a VS Code syntax highlighter extension in etc/ks-vscode.

The AST has just a few concepts: Lambda, Call, Let binding, Conditionals, Constants, Assert, 
Top-level function definitions, and Rewrite rules.

It's also strongly typed, using a bare set of types: Tensor, Tuple, and Values (Float, Integer, String).  However, unlike say MLIR, these are composable, so e.g. we can have a (Tensor 1 (Tuple Float (Tensor 2 Float))).  Tensors are annotated with their "rank" or number of dimensions.

The IR is pure functional, so functions may be called more than once or not at all depending on the use of their outputs.  Values are passed and returned "as if" by value, although the optimizer reserves the right implement this using refcounting/copy/move, as long as the semantics are preserved.

The lisp-like IR is extremely simple -- all the language builtins are in this code:
```scheme
;; Externally defined function "sqrt" returns a Float, takes two Float
(edef atan2 Float (Float Float)) 

#| Block comments
 -- User-defined function myfun 
 takes an Integer and Tensor of (Float Float) pairs
 and returns a pair of String and Float
|#
(def myfun                                       ; function name
  (Tuple String Float)                           ; return type
  ((i : Integer)                                 ; argument 1: int
   (v : Tensor 1 (Tuple Float Float)))           ; argument 2: tensor of tuple
  (assert (gt i 0)                               ; (assert TEST BODY)
     (if (eq i 0)                                ; (if TEST TEXPR FEXPR)
        ; "then" br
        (tuple "fred"                            ; tuple constructor
           (let (tmp (index 0 v))                ; (let (VAR VAL) BODY)
             (mul (get$1$2 tmp) 2.0)) )          ; no builtins -- even mul is a function
        ; "else" br
        (let ((t1 (index 0 v))                   ; (let ((VAR1 VAL1) ... (VARn VALn)) BODY)
              (t2 (myfun (sub i 1) v)))          ; Recursive call to myfun
          t2))))

;; Rewrite rule
(rule "mul.commute"                     ; rule name
    ((a : Float) (b : Float))           ; "template_vars": free variables in the template
    (mul a b)                           ; "template": a pattern to match
    (mul b a))                          ; replacement
; Rules are *not* applied exhaustively, that's very intentionally left up to the compiler.  
; For example the rule above, if matched exhaustively would not terminate.

;; And compilation produces f and various derivatives, as if
(edef rev$myfun ; classic reverse mode.  If f :: S -> T, then rev$f :: (S, dT) -> dS
    (Tuple (Tuple) (Tensor 1 (Tuple Float Float))) ; dS is tangent-type of inputs (dInteger = void)
    ((#|s  |# (Tuple Integer (Tensor 1 (Tuple Float Float)))) ; inputs in a single tuple
     (#|dt |# (Tuple (Tuple) Float)))) ; dT is tangent type of returns

(edef D$myfun  ; linear map as per Elliot.  If f :: S->T, D$f :: S -> (LM dS dT) where LM is linear map
    (LM
      (Tuple (Tuple) (Tensor 1 (Tuple Float Float))) ; dS
      (Tuple (Tuple) Float) ; dT
    )
    (Integer (Tensor 1 (Tuple Float Float)))) ; inputs as normal (not single-tupled)
```
See [the ksc syntax primer](test/ksc/syntax-primer.ks) for a longer
introduction to the `ks` language.  [The ksc test
directory](test/ksc) provides more examples of the constructs
available when writing `.ks` files.


## INSTALLATION/BUILDING

### If you experience any difficulty getting started

Knossos will only be a successful project if the onboarding experience
is straightforward.  We consider any difficulty getting started whilst
following these instructions to be a critical issue that we should fix
urgently.  Therefore if you experience any difficulty getting started
please follow these steps:

1. [File an
issue](https://github.com/microsoft/knossos-ksc/issues/new) with the
title "Difficulty onboarding" that explains as much as possible about
the difficulty you are having.

2. Email knossos@service.microsoft.com with the subject "Urgent:
difficulty onboarding to knossos-ksc" with a link to the new issue you
just filed.

We will respond to you as a matter of top priority.

### Please report your experience onboarding

Please report your experience of onboarding, regardless of whether it
was good or bad.  It is hard to test onboarding automatically and so
we rely on new users to tell us about their experience.  After
following this guide, whether you were successful or not, please

* [File an issue](https://github.com/microsoft/knossos-ksc/issues/new)
  with the title "Experience report: new user onboarding" describing
  how you found your onboarding experience.

Many thanks, the Knossos team.

### Installation

See [ksc](README-ksc.md) or [mlir](README-mlir.md)

### Running the ksc executable

#### Running

Run the `ksc` executable as follows to differentiate, compile and run
a `.ks` program.  This example runs `hello-world.ks`.

```
./build/bin/ksc --compile-and-run \
  --ks-source-file src/runtime/prelude.ks \
  --ks-source-file test/ksc/hello-world.ks \
  --ks-output-file obj/test/ksc/hello-world.kso \
  --cpp-output-file obj/test/ksc/hello-world.cpp \
  --c++ g++ \
  --exe-output-file obj/test/ksc/hello-world.exe
```

or with PowerShell syntax:

```
./build/bin/ksc --compile-and-run `
  --ks-source-file src/runtime/prelude.ks `
  --ks-source-file test/ksc/hello-world.ks `
  --ks-output-file obj/test/ksc/hello-world.kso `
  --cpp-output-file obj/test/ksc/hello-world.cpp `
  --c++ g++ `
  --exe-output-file obj/test/ksc/hello-world.exe
```

Which should produce output like this:
```
read decls
Linted Typechecked defs
Linted Grad
Linted Grad tupled
Linted Optgrad
Linted Optgrad tupled
Linted Diffs
Linted OptDiffs
Linted CSE
Writing to obj/test/ksc/hello-world.kso
Writing to obj/test/ksc/hello-world.cpp
Compiling: g++ -fmax-errors=5 -fdiagnostics-color=always -Wall -Wno-unused -Wno-maybe-uninitialized -Isrc/runtime -O3 -g -std=c++17 -o obj/test/ksc/hello-world.exe obj/test/ksc/hello-world.cpp
Running

Hello world!
If you are seeing this output then knossos-ksc has successfully compiled and run the hello-world.ks program!
```

#### Tests

To run the ksc self-tests use the command line

```
./build/bin/ksc --test --fs-test out.fs
```

(Don't worry if the final test, of `out.fs`, fails.  It is a test for
F#-to-ks, which most users will not have set up.)

#### Generating a `.kso` file from a `.ks` file without differentiating

To generate a `.kso` file from a `.ks` file without differentiating,
i.e. to type check and apply ksc's heuristic optimisations, use the
command line

```
./build/bin/ksc --generate-cpp-without-diffs \
  --ks-source-file src/runtime/prelude.ks \
  --ks-source-file input.ks \
  --ks-output-file output.ks \
  --cpp-output-file output.cpp
```

## KSC Internals

### Syntax of .ks files

In the compiler, the IR is defined in [`Lang.hs`](src/ksc/Lang.hs).
The syntax is defined by the parser in
[`Parse.hs`](src/ksc/Parse.hs) and the pretty-printer in
[`Lang.hs`](src/ksc/Lang.hs).  `testRoundTrip` in
[`Main.hs`](src/ksc/Main.hs) checks that they agree.


## Continuous integration

knossos-ksc has a [continuous integration build set up on Azure
DevOps](https://msrcambridge.visualstudio.com/Knossos/_build?definitionId=609).
To manually run a CI build click "Run pipeline", type the name of your
branch under "Branch/tag", and click "Run".

## Code of Conduct

Collaboration on this project is subject to the [Microsoft Open Source
Code of Conduct](https://opensource.microsoft.com/codeofconduct).
