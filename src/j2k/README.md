#### J2K: Convert Julia to Knossos IR

Julia already offers a rich set of tools (particularly Mike Innes's IRTools) for reflection over methods, so this is a short demo of how that allows translation of Julia code to Knossos IR.

To demo, just `julia --project j2k.jl --input foo1.jl --output test.jl`.  This takes these three method definitions

```julia
f(x) = cos(x) * x

sumsq(xs) = sum(xs.^2)

function foo1(as :: Vector{Float64}, b :: Float64)
    p = length(as)
    p = p - 1
    if p > 0
        if as[1] > 0
            y = [sin(a) for a in as] .* f(b)
        else
            y = [1.1*p, foo1(as[2:end], 1.1)]
        end
    else
        y = -as.*f(b)
    end
    f(sumsq(y)) + 5.5555
end
```

Which converts to this IRTools IR:

```llvm
1: (%1 :: Core.Compiler.Const(foo1, false), %2 :: Array{Float64,1}, %3 :: Float64)
  %4 = Main.length(%2) :: Int64
  %5 = %4 - 1 :: Int64
  %6 = %5 > 0 :: Bool
  br 6 (%2, %3) unless %6
  br 2 (%2, %5, %3)
2: (%35, %36, %37)
  %7 = Base.getindex(%35, 1) :: Float64
  %8 = %7 > 0 :: Bool
  br 4 (%35, %36) unless %8
  br 3 (%35, %37)
3: (%33, %34)
  %9 = Base.Generator(Main.sin, %33) :: Base.Generator{Array{Float64,1},typeof(sin)}
  %10 = Base.collect(%9) :: Array{Float64,1}
  %11 = Main.f(%34) :: Float64
  %12 = Base.broadcasted(Main.:*, %10, %11) :: Base.Broadcast.Broadcasted{Base.Broadcast.DefaultArrayStyle{1},Nothing,typeof(*),Tuple{Array{Float64,1},Float64}}
  %13 = Base.materialize(%12) :: Array{Float64,1}
  br 5 (%13)
4: (%31, %32)
  %14 = 1.1 * %32 :: Float64
  %15 = Base.lastindex(%31) :: Int64
  %16 = 2:%15 :: Core.Compiler.PartialStruct(UnitRange{Int64}, Any[Core.Compiler.Const(2, false), Int64])
  %17 = Base.getindex(%31, %16) :: Array{Float64,1}
  %18 = Main.foo1(%17, 1.1) :: Float64
  %19 = Base.vect(%14, %18) :: Array{Float64,1}
  br 5 (%19)
5: (%20 :: Array{Float64,1})
  br 7 (%20)
6: (%29, %30)
  %21 = -%29 :: Array{Float64,1}
  %22 = Main.f(%30) :: Float64
  %23 = Base.broadcasted(Main.:*, %21, %22) :: Base.Broadcast.Broadcasted{Base.Broadcast.DefaultArrayStyle{1},Nothing,typeof(*),Tuple{Array{Float64,1},Float64}}
  %24 = Base.materialize(%23) :: Array{Float64,1}
  br 7 (%24)
7: (%25 :: Array{Float64,1})
  %26 = Main.sumsq(%25) :: Float64
  %27 = Main.f(%26) :: Float64
  %28 = %27 + 5.5555 :: Float64
  return %28
```

  And thence to this Knossos IR (with def->defun to use the lisp syntax colorizer)

  ```lisp
(defun b7 Any ((%25 : Array{Float64,1}))
     (let (_26 (Main.sumsq _25))
     (let (_27 (Main.f _26))
	 (let (_28 (Main.+ _27 5.5555))
	   (%28)))))

(defun b6 Any ((%29 : Any) (%30 : Any))
     (let (_21 (Main.- _29))
     (let (_22 (Main.f _30))
	 (let (_23 (Base.broadcasted Main.* _21 _22))
	   (let (_24 (Base.materialize _23))
	     (b7 %24))))))

(defun b5 Any ((%20 : Array{Float64,1}))
     (b7 %20))

(defun b4 Any ((%31 : Any) (%32 : Any))
     (let (_14 (Main.* 1.1 _32))
     (let (_15 (Base.lastindex _31))
	 (let (_16 (Main.: 2 _15))
	 (let (_17 (Base.getindex _31 _16))
	 (let (_18 (Main.foo1 _17 1.1))
	 (let (_19 (Base.vect _14 _18))
		 (b5 %19))))))))

(defun b3 Any ((%33 : Any) (%34 : Any))
     (let (_9 (Base.Generator Main.sin _33))
     (let (_10 (Base.collect _9))
	 (let (_11 (Main.f _34))
	 (let (_12 (Base.broadcasted Main.* _10 _11))
	 (let (_13 (Base.materialize _12))
	       (b5 %13)))))))

(defun b2 Any ((%35 : Any) (%36 : Any) (%37 : Any))
     (let (_7 (Base.getindex _35 1))
     (let (_8 (Main.> _7 0))
	 (if _8
	     (b4 %35 %36)
	   (b3 %35 %37)))))

(defun foo1 Any ((%2 : Array{Float64,1}) (%3 : Float64))
     (let (_4 (Main.length _2))
     (let (_5 (Main.- _4 1))
	 (let (_6 (Main.> _5 0))
	   (if _6
	       (b6 %2 %3)
	     (b2 %2 %5 %3))))))
```

This can also be connected to the overall tools, from the project root

```
julia --project=./src/j2k/ ./src/j2k/j2k.jl --input ./test/j2k/test0.jl --output obj/test/j2k/j2k_test0.ks
```

then (in PowerShell style)

```powershell
./ksc --compile-and-run `
  --ks-source-file src/runtime/prelude.ks `
  --ks-source-file obj/test/j2k/j2k_test0.ks `
  --ks-output-file obj/test/j2k/j2k_test0.kso `
  --cpp-output-file obj/test/j2k/j2k_test0.cpp `
  --c++ g++ `
  --exe-output-file obj/test/j2k/j2k_test0.exe
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