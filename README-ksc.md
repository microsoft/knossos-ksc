#### KSC Installation

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
choco install ghc --version 8.6.5 cabal --version 3.2.0.0 -y
cabal v2-update
choco install mingw --version 7.3.0 -y
choco install msys2
refreshenv
```

#### Ubuntu

You ought to use Ubuntu version >= 18.04 because older Ubuntus don't
have g++ >= 7.  Ubuntu 18.04 and 20.04 under WSL2 work perfectly fine.  The
simplest way to get ghc and cabal is to install specific versions
from a [specific personal package archive
(PPA)](https://launchpad.net/~hvr/+archive/ubuntu/ghc/) as detailed
below.  (Please note that cabal from the PPA
[will not work on WSL1](https://github.com/haskell/cabal/issues/6551)).

```sh
sudo add-apt-repository ppa:hvr/ghc
sudo apt-get update
sudo apt install ghc-8.6.5 cabal-install-3.0 g++
/opt/cabal/bin/cabal-3.0 v2-update
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
to put it on their `PATH` with

```
export PATH=/opt/ghc/8.6.5/bin:/opt/cabal/3.0/bin:$PATH
```

or use the following, more explicit, command line.

```
/opt/cabal/bin/cabal-3.0 v2-build --ghc-option=-Wwarn --with-ghc /opt/ghc/bin/ghc-8.6.5
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
cabal v2-install --installdir=build/bin --overwrite-policy=always --install-method=copy
```

Those who installed cabal and ghc via the PPA might need to use the
following, more explicit, command line at the last stage

```
/opt/cabal/bin/cabal-3.0 v2-install --with-ghc /opt/ghc/bin/ghc-8.6.5 --installdir=build/bin --overwrite-policy=always --install-method=copy
```

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
