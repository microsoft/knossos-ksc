#### TS2K: Convert TorchScript to Knossos IR

TorchScript https://pytorch.org/tutorials/beginner/Intro_to_TorchScript_tutorial.html provides a way to get the full code graph which we can convert to Knossos IR.


To demo, just `python ts2k.py`. (TODO: allow it to take arguments)

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

Which converts to this TorchScript graph:

```
graph(%x.1 : Tensor):
  %3 : int = prim::Constant[value=1]()
  %2 : float = prim::Constant[value=4]() # ts2k.py:19:15
  %4 : Tensor = aten::add(%x.1, %2, %3) # ts2k.py:19:11
  return (%4)

graph():
  %16 : str = prim::Constant[value="Hello world from TorchScript -> Knossos!"]() # ts2k.py:25:10
  %14 : Function = prim::Constant[name="f"]()
  %11 : bool = prim::Constant[value=0]()
  %9 : None = prim::Constant()
  %0 : float = prim::Constant[value=1.2]() # ts2k.py:23:23
  %1 : float = prim::Constant[value=3.4]() # ts2k.py:23:28
  %2 : float = prim::Constant[value=5.6]() # ts2k.py:23:33
  %4 : float = prim::Constant[value=2.3]() # ts2k.py:23:40
  %5 : float = prim::Constant[value=4.5]() # ts2k.py:23:45
  %6 : float = prim::Constant[value=6.7]() # ts2k.py:23:50
  %3 : float[] = prim::ListConstruct(%0, %1, %2)
  %7 : float[] = prim::ListConstruct(%4, %5, %6)
  %8 : float[][] = prim::ListConstruct(%3, %7)
  %t.1 : Tensor = aten::tensor(%8, %9, %9, %11) # ts2k.py:23:8
  %t2 : Tensor = prim::CallFunction(%14, %t.1) # ts2k.py:24:9
   = prim::Print(%16) # ts2k.py:25:4
  return (%9)
```

  And thence to this Knossos IR (TODO)

  ```lisp
(def main Integer () ((print "\nHello world from TorchScript -> Knossos!\n") (print "\nHello world from TorchScript -> Knossos!\n")))
```

This can also be connected to the overall tools, from the project root

```
python src\ts2k\ts2k.py > obj/test/ts2k/ts2k_test.ks
```

then (in PowerShell style)

```powershell
./ksc --compile-and-run `
  --ks-source-file src/runtime/prelude.ks `
  --ks-source-file obj/test/ts2k/ts2k_test.ks `
  --ks-output-file obj/test/ts2k/ts2k_test.kso `
  --cpp-output-file obj/test/ts2k/ts2k_test.cpp `
  --c++ g++ `
  --exe-output-file obj/test/ts2k/ts2k_test.exe
```

which currently fails, but we can work on next

```
read decls
ksc.exe: Failed parse: (line 1, column 1):
unexpected '-'
expecting end of input or "("
CallStack (from HasCallStack):
  error, called at src/ksc\Parse.hs:124:36 in main:Parse
```