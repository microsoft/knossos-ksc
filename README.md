# Knossos-KSC  

#### Compile a lisp-like IR with automatic differentiation and user-defined rewrites.

This project is a functional compiler and code-gen tool that will
accelerate writing AI algorithms as well as making them easier.   The core is a lisp-like IR that can be translated from high-level languages, and can be linked to a variety of backends to generate code.

Currently implemented frontends are
 * Julia: j2ks
 * F#: f2k
 * TorchScript: ts2k
 * KS-Lisp: The IR itself is exchanged in a lisp-like text format (see below).  

Current backends:
 * CPU/C++: In src/ksc and src/runtime
 * GPU/Futhark: Also in src/ksc
 * MLIR: In mlir

Current transformers:
 * KSC: Various Autodiff and optimization transforms, in Haskell 

### KS-Lisp: A low-sugar IR

The text form of the IR is "low sugar", focused more on simplicty than user-friendliness.
However it *is* human writable, and lispers in particular may like to play with it.
There's a VS Code syntax highlighter extension in etc/ks-vscode.

The AST has just a few concepts: Lambda, Call, Let binding, Conditionals, Constants, Assert, 
Top-level function definitions, and Rewrite rules.

It's also strongly typed, using a bare set of types: Vector, Tuple, and Values (Float, Integer, String).  However, unlike say MLIR, these are composable, so e.g. we can have a (Vec (Tuple Float (Vec Float))).
[It's a TODO to replace Vec with Tensor]

The IR is pure functional, so functions may be called more than once or not at all depending on the use of their outputs.  Values are passed and returned "as if" by value, although the optimizer reserves the right implement this using refcounting/copy/move, as long as the semantics are preserved.

The lisp-like IR is extremely simple -- all the language builtins are in this code:
```scheme
;; Externally defined function "sqrt" returns a Float, takes two Float
(edef atan2 Float (Float Float)) 

#| Block comments
 -- User-defined function f 
 takes an Integer and Vec of (Float Float) pairs
 and returns a pair of String and Float
|#
(def myfun                                       ; function name
  (Tuple String Float)                           ; return type
  ((i : Integer) (v : Vec (Tuple Float Float)))  ; arguments
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
    ((a : Float) (b : Float))           ; free variables
    (mul a b)                           ; pattern to match
    (mul b a))                          ; replacement
; Rules are *not* applied exhaustively, that's very intentionally left up to the compiler.  
; For example the rule above, if matched exhaustively would not terminate.

;; And compilation produces f and various derivatives, as if
(edef rev$myfun ; classic reverse mode.  If f :: S -> T, then rev$f :: (S, dT) -> dS
    (Tuple (Tuple) (Vec (Tuple Float Float))) ; dS is tangent-type of inputs (dInteger = void)
    ((#|s  |# (Tuple Integer (Vec (Tuple Float Float)))) ; inputs in a single tuple
     (#|dt |# (Tuple (Tuple) Float)))) ; dT is tangent type of returns

(edef D$myfun  ; linear map as per Elliot.  If f :: S->T, D$f :: S -> (LM dS dT) where LM is linear map
    (LM
      (Tuple (Tuple) (Vec (Tuple Float Float))) ; dS
      (Tuple (Tuple) Float) ; dT
    )
    (Integer (Vec (Tuple Float Float)))) ; inputs as normal (not single-tupled)
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

### Installing dependencies

Knossos `ksc` requires reasonably up-to-date versions of ghc, cabal
and g++.   The following are sufficient

* ghc version >= 8.4
* cabal version >= 3.0
* g++ version >= 7

This section describes how to get them.

#### Windows
Install [Chocolatey](https://chocolatey.org/), then:
```cmd
choco install ghc --version 8.6.5 -y
cabal v2-update
choco install mingw --version 7.3.0 -y
choco install msys2
refreshenv
```

#### Ubuntu

You ought to use Ubuntu version >= 18.04 because older Ubuntus don't
have g++ >= 7.  Ubuntu 18.04 under WSL works perfectly fine.  The
simplest way to get ghc and cabal is to install specific versions
using [ghcup](https://gitlab.haskell.org/haskell/ghcup) as detailed
below.

```sh
sudo apt-get update
sudo apt-get install build-essential libgmp-dev

# NB Installing 8.6.5 has copious ouput
curl https://raw.githubusercontent.com/haskell/ghcup/c2bc5941f076f1fa9c62169f6217acac8dd62fc8/ghcup > ghcup
sh ./ghcup install 8.6.5
sh ./ghcup install-cabal 3.0.0.0
~/.ghcup/bin/cabal v2-update
```

### Building

Build knossos in the `knossos-ksc` folder as follows.  If the
versions of ghc and cabal you installed above are on your `PATH` then
it will be sufficient to do

```sh
cd knossos-ksc
cabal v2-build --ghc-option=-Wwarn
```

`choco` users on Windows should find that cabal and ghc are already on
their `PATH` so that command will run fine.  Ubuntu users might need
to use the following, more explicit, command line.

```
~/.ghcup/bin/cabal v2-build --ghc-option=-Wwarn --with-ghc ~/.ghcup/ghc/8.6.5/bin/ghc
```

On the first run, it will build a lot of packages, which will look a bit like

```
- call-stack-0.2.0 (lib) (requires build)
...
Starting     setenv-0.1.1.3 (lib)
Starting     hspec-discover-2.7.1 (lib)
...
Building     primitive-0.7.0.0 (lib)
...
Installing   setenv-0.1.1.3 (lib)
...
Completed    setenv-0.1.1.3 (lib)
...
```

Then, and on subsequent runs, it will build knossos, which should have output as follows
```
Building executable 'ksc' for knossos-0.0.0.1..
[ 1 of 17] Compiling KMonad           ( src/ksc/KMonad.hs, ..../knossos-ksc/dist-newstyle/build/x86_64-linux/ghc-8.6.5/knossos-0.0.0.1/x/ksc/build/ksc/ksc-tmp/KMonad.o )
...
[17 of 17] Compiling Main             ( src/ksc/Main.hs, ..../knossos-ksc/dist-newstyle/build/x86_64-linux/ghc-8.6.5/knossos-0.0.0.1/x/ksc/build/ksc/ksc-tmp/Main.o )
Linking ..../knossos-ksc/dist-newstyle/build/x86_64-linux/ghc-8.6.5/knossos-0.0.0.1/x/ksc/build/ksc/ksc
```

### Installing the ksc executable

To create the `ksc` executable run the following.  If the versions of
ghc and cabal you installed above are on your `PATH` then it will be
sufficient to do

```
mkdir -p build/bin  # Share build dir with ksc-mlir
cabal v2-install --installdir=build/bin --overwrite-policy=always
```

Those who installed cabal and ghc via ghcup might need to use the
following, more explicit, command line at the last stage

```
~/.ghcup/bin/cabal v2-install --with-ghc ~/.ghcup/ghc/8.6.5/bin/ghc --installdir=build/bin --overwrite-policy=always
```

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

### Compiler pipeline

The compiler works by parsing the source code, generating forward and
reverse mode automatic derivatives, and then applying some
optimisations before emitting the code to backend.

The main backend is C++ (defined in [`Cgen.hs`](src/ksc/Cgen.hs)).
It depends on a small runtime (defined in
[`src/runtime/knossos.h`](src/runtime/knossos.h)) which provides a
bump-allocated vector
implementation, implementations of primitives, and a very small
standard library called the "prelude".

We also have a [Futhark](https://futhark-lang.org/) backend, but most
of our efforts are concentrated on C++ at the moment.

## Code of Conduct

Collaboration on this project is subject to the [Microsoft Open Source
Code of Conduct](https://opensource.microsoft.com/codeofconduct).
