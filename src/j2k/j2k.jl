# Julia to Knossos
using Zygote
using IRTools
import Base.show

### TODOS
# 1. Nice printing :)
# 2. Instead of env_args, collect free vars of expressions, and pass those down explicitly
#    (e.g. now all basic blocks get passed all incoming args, which is messy, and will 
#    cause a few redunant derivative calculations.  Of course Knossos can just rewrite 
#    them away, but it's nice to remove obvious cruft.
# 3. Emit fully monomorphized (essentially name-mangled) function names, e.g. "mul@Float64,Array{Float64}"
# 4. Handle unhandled constructs

nl = "\n"

blockid(id::Int) = string("b",id)

struct Ex 
    args :: Array{Any}
end

head(e) = e.args[1]

function show(io::IO, ex::Ex)
    print(io, "(")
    args = ex.args
    if length(args) > 0
        for a in args[1:end-1]
            print(io, a)
            print(io, " ")
        end
        print(io, args[end])
    end
    print(io, ")")
end

sexp(args...) = Ex([args...])

function make_sexp(x :: Any)
    sexp("[UNK", typeof(x), ":", x, "]")
end

function make_sexp(x :: Number)
    x
end

function make_sexp(x :: IRTools.Variable)
    string("v",x)
end

function make_sexp(x :: GlobalRef)
    string(x.mod, ".", x.name)
end

function make_sexp(x :: Expr)
    if x.head == :call
        sexp(map(make_sexp, x.args)...)
    else
        sexp("[EXPR<", x.head, ">:", x, "]")
    end
end

function make_let(x :: IRTools.Variable, st :: IRTools.Statement, body) 
    if isnothing(st.expr)
        ex = sexp("!!!") # When does this happen?
    else
        ex = make_sexp(st.expr)
    end
    if isnothing(x)
        sexp("nil", ex)
    else
        sexp("let", sexp(string("v%", x.id),ex), nl, body)
    end
end

function make_lets(let_args, body)
    n = length(let_args)
    if n == 0
        return body
    end

    x,st = let_args[1]
    body = make_lets(let_args[2:end], body)
    make_let(x, st, body)
end

# struct IRTools.Branch
#     condition :: Any
#     block     :: Int64
#     args      :: Array{Any,1}

function make_sexp_br(env_args, id :: Int, br :: IRTools.Branch)
    if br.block == 0
        sexp(br.args...)
    else
        sexp(blockid(br.block), map(head, env_args)..., br.args...)
    end
end

function make_if(cond, t, f)
    sexp("if", cond, nl,
         t, nl,
         f)
end

function make_sexp_brs(env_args, id :: Int, br :: IRTools.Branch)
    # The last branch in a list
    if isnothing(br.condition)
        # unconditional... 
        make_sexp_br(env_args, id, br)
    else
        # if conditional, is either the br or a fallthrough
        make_if(br.condition,
                sexp(blockid(id+1), map(head, env_args)...), 
                make_sexp_br(env_args, id, br))
    end
end

function make_sexp_brs(env_args, id :: Int, br :: IRTools.Branch, tail...)
    # Must be conditional if tail nonempty
    @assert !isnothing(br.condition)
    make_if(br.condition, 
            make_sexp_br(env_args, id, br), 
            make_sexp_brs(env_args, id, tail...))
end

function block_args(b :: IRTools.Block)
    ats = zip(IRTools.arguments(b),IRTools.argtypes(b))
    [sexp(a,":",t) for (a,t) in ats]
end

function make_sexp(env_args, env_args2, b :: IRTools.Block)
    args = sexp(env_args..., block_args(b)...)

    if false
        for (x,t) in b
            if x == undef
                println("T>>>", t)
            else
                println(x," = ", t)
            end
        end
    end

    branches = make_sexp_brs(env_args2, b.id, IRTools.branches(b)...)

    let_args = [(x,t) for (x,t) in b if x != undef]
    body = make_lets(let_args, branches)
            
    se = sexp("def",blockid(b.id), 
              "Any",
              args, nl,
              body)
    se
end

function make_sexps(ir :: IRTools.IR)
    blocks = IRTools.blocks(ir)
    env_args = block_args(blocks[1])

    for b in reverse(blocks[2:end])
        se = make_sexp(env_args, env_args, b)
        println(se,"\n")
    end
    se = make_sexp([], env_args, blocks[1])
    println(se)
end

###################
### Test

print("---\n")
f(x) = cos(x) * x

sumsq(xs) = sum(xs.^2)

function foo1(as,b)
    if length(as) > 1
        y = [a*sin(a) for a in as] .* f(b)
    else
        y = -as.*f(b)
    end
    f(sumsq(y)) + 5
end

ir = IR(IRTools.typed_meta(Tuple{typeof(foo1),Array{Float64,1},Float64}))
println(ir)
println("--------------------")
make_sexps(ir)
