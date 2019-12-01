# Julia to Knossos
using Zygote
using IRTools
import Base.show

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
    x
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

function make_sexp_brs(env_args, id :: Int, br :: IRTools.Branch)
    if isnothing(br.condition)
        make_sexp_br(env_args, id, br)
    else
        sexp("if", br.condition, nl,
             sexp(blockid(id+1), map(head, env_args)...), nl,
             make_sexp_br(env_args, id, br))
    end
end

function make_sexp_brs(env_args, id :: Int, br :: IRTools.Branch, tail...)
    # Must be conditional if tail nonempty
    @assert !isnothing(br.condition)
    sexp("if", br.condition, nl, make_sexp_br(env_args, id, br), nl, make_sexp_brs(env_args, id, tail...))
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

function foo1(a,b)
    if a > 0
        y = sin(a) * f(b)
    else
        y = -a*f(b)
    end
    f(y) + 5
end

ir = @code_ir foo1(1.1,2.2)
println(ir)

make_sexps(ir)
