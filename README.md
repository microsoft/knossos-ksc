# Compiler with automatic differentiation

This project is a functional compiler and code-gen tool that will
accelerate writing AI algorithms as well as making them easier.  The
output of code-gen is C++ code. We envisage that it will have
interfaces for popular languages such as Python, Julia and F# which
will make it easier for a wide variety of programmers to leverage the
benefits.

## Read this first

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

## Installing dependencies

Knossos `ksc` requires reasonably up-to-date versions of ghc, cabal
and g++.   The following are sufficient

* ghc version >= 8.4
* cabal version >= 2.0
* g++ version >= 7

This section describes how to get them.

### Windows
Install [Chocolatey](https://chocolatey.org/), then:
```cmd
choco install ghc --version 8.6.5 -y
cabal v2-update
choco install mingw --version 7.3.0 -y
refreshenv
```

### Ubuntu

You ought to use Ubuntu version >= 18.06 because older Ubuntus don't
have g++ >= 7.  Ubuntu 18.06 under WSL works perfectly fine.  The
simplest way to get ghc and cabal is to install specific versions from
a specific package archive, as detailed below.

```sh
# ppa:hvr/ghc is an Ubuntu personal package archive that allows you
# to install specific versions of ghc and cabal.  More information is
# available at
#
#     https://launchpad.net/~hvr/+archive/ubuntu/ghc/
sudo add-apt-repository ppa:hvr/ghc
sudo apt-get update
sudo apt install ghc-8.6.5 cabal-install-2.4 g++
/opt/cabal/bin/cabal-2.4 v2-update
```

### Cloning knossos

```
git clone https://github.com/microsoft/knossos-ksc
cd knossos-ksc
```

## Running

Start the ghc REPL in the `knossos-ksc` folder as follows.  If the
versions of ghc and cabal you installed above are on your `PATH` then
it will be sufficient to do

```sh
cabal v2-repl --ghc-option=-Wwarn
```

`choco` users on Windows should find that cabal and ghc are already on
their `PATH` so that command will run fine.  Ubuntu users might need
to use the following, more explicit, command line.

```
/opt/cabal/2.4/bin/cabal v2-repl --ghc-option=-Wwarn --with-ghc=/opt/ghc/bin/ghc-8.6.5
```

It will build a lot of packages, which will look a bit like

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

Then it will present you with a prompt at which you should issue the
following two commands

```
:l src/ksc/Main
displayCppGenCompileAndRunWithOutput "g++" Nothing "test/ksc/hello-world"
```

The output will look a little bit like this.

<pre>
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Prelude> :l src/ksc/Main
[ 1 of 14] Compiling KMonad           ( src/ksc/KMonad.hs, interpreted )
[ 2 of 14] Compiling Lang             ( src/ksc/Lang.hs, interpreted )
[ 3 of 14] Compiling LangUtils        ( src/ksc/LangUtils.hs, interpreted )
[ 4 of 14] Compiling Prim             ( src/ksc/Prim.hs, interpreted )
[ 5 of 14] Compiling OptLet           ( src/ksc/OptLet.hs, interpreted )
[ 6 of 14] Compiling Parse            ( src/ksc/Parse.hs, interpreted )
[ 7 of 14] Compiling Rules            ( src/ksc/Rules.hs, interpreted )
[ 8 of 14] Compiling Cgen             ( src/ksc/Cgen.hs, interpreted )
[ 9 of 14] Compiling Annotate         ( src/ksc/Annotate.hs, interpreted )
[10 of 14] Compiling Opt              ( src/ksc/Opt.hs, interpreted )
[11 of 14] Compiling ANF              ( src/ksc/ANF.hs, interpreted )
[12 of 14] Compiling CSE              ( src/ksc/CSE.hs, interpreted )
[13 of 14] Compiling AD               ( src/ksc/AD.hs, interpreted )
[14 of 14] Compiling Main             ( src/ksc/Main.hs, interpreted )
Ok, modules loaded: AD, ANF, Annotate, CSE, Cgen, KMonad, Lang, LangUtils, Main, Opt, OptLet, Parse, Prim, Rules.
*Main> 
*Main> <b>-- Compile hello-world.ks to hello-world.cpp and run</b>
*Main> <b>displayCppGenCompileAndRunWithOutput "g++" Nothing "test/ksc/hello-world"</b>
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
Done

Hello world!
If you are seeing this output then knossos-ksc has successfully compiled and run the hello-world.ks program!
</pre>

## Code of Conduct

Collaboration on this project is subject to the [Microsoft Open Source
Code of Conduct](https://opensource.microsoft.com/codeofconduct).
