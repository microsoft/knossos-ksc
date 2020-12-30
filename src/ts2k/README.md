#### TS2K: Convert TorchScript to Knossos IR

TorchScript https://pytorch.org/tutorials/beginner/Intro_to_TorchScript_tutorial.html provides a way to get the full code graph which we can convert to Knossos IR.


To demo, create a test.py file

```python
@torch.jit.script
def f(x):
    return x + 4.0

@torch.jit.script
def main():
    t = torch.tensor([[1.2, 3.4, 5.6], [2.3, 4.5, 6.7]])
    t2 = f(t)
    print("Hello world from TorchScript -> Knossos!")
```

and run with

`python src/ts2k/ts2k.py --input_file=test.py --output_file=test.ks`

Which converts to this TorchScript graph:

```
#| -------- Graph ----------graph(%x.1 : Tensor):
  %3 : int = prim::Constant[value=1]()
  %2 : float = prim::Constant[value=4]() # src\ts2k\ts2k.py:27:15
  %4 : Tensor = aten::add(%x.1, %2, %3) # src\ts2k\ts2k.py:27:11
  return (%4)

graph():
  %16 : str = prim::Constant[value="Hello world from TorchScript -> Knossos!"]() # src\ts2k\ts2k.py:33:10
  %14 : Function = prim::Constant[name="f"]()
  %11 : bool = prim::Constant[value=0]()
  %9 : None = prim::Constant()
  %0 : float = prim::Constant[value=1.2]() # src\ts2k\ts2k.py:31:23
  %1 : float = prim::Constant[value=3.4]() # src\ts2k\ts2k.py:31:28
  %2 : float = prim::Constant[value=5.6]() # src\ts2k\ts2k.py:31:33
  %4 : float = prim::Constant[value=2.3]() # src\ts2k\ts2k.py:31:40
  %5 : float = prim::Constant[value=4.5]() # src\ts2k\ts2k.py:31:45
  %6 : float = prim::Constant[value=6.7]() # src\ts2k\ts2k.py:31:50
  %3 : float[] = prim::ListConstruct(%0, %1, %2)
  %7 : float[] = prim::ListConstruct(%4, %5, %6)
  %8 : float[][] = prim::ListConstruct(%3, %7)
  %t.1 : Tensor = aten::tensor(%8, %9, %9, %11) # src\ts2k\ts2k.py:31:8
  %t2 : Tensor = prim::CallFunction(%14, %t.1) # src\ts2k\ts2k.py:32:9
   = prim::Print(%16) # src\ts2k\ts2k.py:33:4
  return (%9)
-------- |#
```

  And thence to this Knossos IR

  ```lisp
(def main Integer () (
 let ((
 _16 "Hello world from TorchScript -> Knossos!") (
 _14 "FUNCTIONCALL") (
 _11 0.0) (
 _9 0.0) (
 _0 1.2) (
 _1 3.4) (
 _2 5.6) (
 _4 2.3) (
 _5 4.5) (
 _6 6.7)     ) 
 (print _16)))
```

This can also be connected to the overall tools, from the project root

```
python src\ts2k\ts2k.py --input_file=test/ts2k/test0.py --output_file=obj/test/ts2k/ts2k_test.ks
```

then (in PowerShell style)

```powershell
./build/bin/ksc --compile-and-run `
  --ks-source-file src/runtime/prelude.ks `
  --ks-source-file obj/test/ts2k/ts2k_test.ks `
  --ks-output-file obj/test/ts2k/ts2k_test.kso `
  --cpp-output-file obj/test/ts2k/ts2k_test.cpp `
  --c++ g++ `
  --exe-output-file obj/test/ts2k/ts2k_test.exe
```

which compiles and runs

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
Writing to obj/test/ts2k/ts2k_test.kso
Writing to obj/test/ts2k/ts2k_test.cpp
Compiling: g++ -fmax-errors=5 -fdiagnostics-color=always -Wall -Wno-unused -Wno-maybe-uninitialized -Isrc/runtime -O3 -g -std=c++17 -o obj/test/ts2k/ts2k_test.exe obj/test/ts2k/ts2k_test.cpp
Running
Hello world from TorchScript -> Knossos!
```

#### How TorchScript JIT works

[`torch.jit.script`](https://github.com/pytorch/pytorch/blob/f6f1384811b9cc722f650ed9ead8ee99938c009a/torch/jit/__init__.py#L1154)
[calls
`torch.jit.frontend.get_jit_def`](https://github.com/pytorch/pytorch/blob/f6f1384811b9cc722f650ed9ead8ee99938c009a/torch/jit/__init__.py#L1330)
which [loads the original source
code](https://github.com/pytorch/pytorch/blob/f6f1384811b9cc722f650ed9ead8ee99938c009a/torch/jit/frontend.py#L125-L141)
so that it can translate it using the
[`ExprBuilder`](https://github.com/pytorch/pytorch/blob/f6f1384811b9cc722f650ed9ead8ee99938c009a/torch/jit/frontend.py#L375)
and
[`StmtBuilder`](https://github.com/pytorch/pytorch/blob/f6f1384811b9cc722f650ed9ead8ee99938c009a/torch/jit/frontend.py#L255)
classes.
