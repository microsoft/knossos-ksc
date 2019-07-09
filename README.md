# Compiler with automatic differentiation

This project is a functional compiler and code-gen tool that will
accelerate writing AI algorithms as well as making them easier.  The
output of code-gen is C++ code. We envisage that it will have
interfaces for popular languages such as Python, Julia and F# which
will make it easier for a wide variety of programmers to leverage the
benefits.

### Installing

#### Ubuntu
```sh
sudo apt install ghc cabal-install
cabal update
cabal install hspec parsec mtl hashable filepath
git clone <knossos repo>
cd ksc
```

#### Windows
Install [Chocolatey](https://chocolatey.org/), then:
```cmd
choco install ghc --version 8.4.4 -y
cabal update
cabal install hspec parsec mtl hashable filepath
choco install mingw --version 7.3.0 -y
refreshenv
git clone <knossos repo>
cd ksc
```

### Running

Start ghci in this folder:

<pre>
$ <b>ghci -isrc/ksc</b>
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
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
*Main> <b>-- Compile test1.ks to test1.cpp and run</b>
*Main> <b>doallC "g++" 0 "test/ksc/test1"</b>
read decls

----------------------------
Typechecked defs
----------------------------

def a:Float [TFun TypeFloat (Fun (UserFun "a"))] (s$x:Float)
  = 3.0 * s$x

...snip...

def f1:Float [TFun TypeFloat (Fun (UserFun "f1"))]
  (s$x:Float, s$y:Float)
  = a( 5.0 * s$x ) / b( s$y )
Apply not optimized: D$a( s$z )
Apply not optimized: D$b( s$y )
NOTE: Unmatched call {TFun TypeFloat (DrvFun (UserFun "f") Fwd)}
Writing to obj/test/ksc/test1.kso
Writing to obj/test/ksc/test1.cpp
<i>Compiling: g++-7 -fmax-errors=5 -Wall -Isrc/runtime -O -g -std=c++17 obj/test/ksc/test1.cpp -o obj/test/ksc/test1.exe</i>
Running
Done
1.95652
----
0
----
HCat(Scale(0.454545),Scale(-0.227273))
----
2.42
----
-0.227262
----
-0.227273
*Main>
*Main> <b>"Hooray!"</b>
"Hooray!"
</pre>

### Code of Conduct

Collaboration on this project is subject to the [Microsoft Open Source
Code of Conduct](https://opensource.microsoft.com/codeofconduct).
