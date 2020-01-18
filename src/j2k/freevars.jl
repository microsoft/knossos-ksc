using IRTools

## Get set of freeVars from an IR block
const VarSet = Set{IRTools.Variable}
function union!(s :: Set{T}, ts) where {T}
    for t in ts
        push!(s, t)
    end
end

function freevars(env :: VarSet, x :: Any)
    println("[FREEVARS UNK", typeof(x), ":", x, "]")
end

function freevars(env :: VarSet, x :: Number)
    VarSet()
end

function freevars(env :: VarSet, x :: IRTools.Variable)
    if x in env
        VarSet()
    else
        VarSet([x])
    end
end

function freevars(env :: VarSet, x :: GlobalRef)
    VarSet()
end

function freevars(env :: VarSet, x :: Expr)
    if x.head == :call
        out = VarSet()
        for a in x.args
            union!(out, freevars(env, a))
        end
        out
    else
        println("FREEVARS[EXPR<", x.head, ">:", x, "]")
    end
end

function freevars(env :: VarSet, b :: IRTools.Block)
    args = VarSet(IRTools.arguments(b))
    env = union(env, args)
    out = VarSet()
    for (x,st) in b
        if x != undef
            push!(env, x)
        end
        union!(out, freevars(env, st.expr))
    end
    out
end

freevars(x) = freevars(VarSet(), x)

###################
### Test freevars

using Test

function test_freevars_1(x,y)
    # Block 1: Args (%1,%2,%3), FV: ()
    a = sin(x)        # %4
    b = a+y           # %5

    if x > 3          # %6 
        # Block 2: Args (), FV %5
        c = cos(b)    # %7
    else
        # Block 3: Args (), FV %3, %4
        ty = tan(y)   # %8
        c = a + ty    # %9
    end

    # Block 4: Args %10, FV %4
    d = c + a         # %11
    e = atan(d)       # %12
end

ir = @code_ir test_freevars_1(4,5)

set(l) = VarSet(map(IRTools.var, l))
fv(i) = freevars(IRTools.block(ir,i))
@test fv(1) == set([])
@test fv(2) == set([5])
@test fv(3) == set([3,4])
@test fv(4) == set([4])
