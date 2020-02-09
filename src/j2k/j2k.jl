# Julia to Knossos
using Zygote
using IRTools
import Base.show
import IRTools: isconditional

### TODOS
# 1. Need to propagate types 
#    (still some Anys in basic block defs)
# 2. Nice printing :)

# Newline constant for s-expr printing
nl = "\n"
tab = "\t"

# Simple s-expression structure
struct SExpr
    terms :: Array{Any}
end

# Return the head of an s-expression
head(e) = e.terms[1]
tail(e) = SExpr(e.terms[2:end])

# Print an s-expression
function show(io::IO, ex::SExpr)
    print(io, "(")
    terms = ex.terms
    if length(terms) > 0
        for a in terms[1:end-1]
            print(io, a)
            print(io, " ")
        end
        print(io, terms[end])
    end
    print(io, ")")
end

sexp(terms...) = SExpr([terms...])

## IR utilities
blockid(id::Int) = string("b",id)

    
## make_sexp converts IR to s-expressions
function make_sexp(x :: Any)
    sexp("[UNK", typeof(x), ":", x, "]")
end

function make_sexp(x :: Number)
    x
end

function make_sexp(x :: IRTools.Variable)
    string("_", x.id)
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
        sexp("let", sexp(make_sexp(x),ex), nl, body)
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

function make_if(cond, texpr, fexpr)
    sexp("if", make_sexp(cond), nl,
         tab, texpr, nl,
         tab, fexpr)
end

# struct IRTools.Branch
#     condition :: Any
#     block     :: Int64
#     args      :: Array{Any,1}

function make_sexp_branch(id :: Int, br :: IRTools.Branch)
    if br.block == 0
        sexp(br.args...)
    else
        sexp(blockid(br.block), br.args...)
    end
end

function make_sexp_branches(ir :: IRTools.IR, id :: Int, br :: IRTools.Branch, tail...)
    # Must be conditional if tail nonempty
    @assert isconditional(br)
    make_if(br.condition, 
            make_sexp_branch(id, br), 
            make_sexp_branches(ir, id, tail...))
end

function make_sexp_branches(ir :: IRTools.IR, id :: Int, br :: IRTools.Branch)
    # The last branch in a list
    if isconditional(br)
        # if conditional, is either the br or a fallthrough
        make_if(br.condition,
                sexp(blockid(id+1), IRTools.arguments(ir.blocks[id])), 
                make_sexp_branch(id, br))
    else
        # unconditional... 
        make_sexp_branch(id, br)
    end
end

function block_args(b :: IRTools.Block)
    ats = zip(IRTools.arguments(b),IRTools.argtypes(b))
    [sexp(a,":",t) for (a,t) in ats]
end

# struct IRTools.Block
#    arguments   4: (%1, %2)
#    argtypes
#    statements   %1 = e
#    branches     br n if e     -- if e, call block n, else fallthrough
function make_def(is_toplevel :: Bool, name, b :: IRTools.Block)
    args = sexp(block_args(b)...)
    if is_toplevel
        # Strip the method name for top-level functions
        args = tail(args) 
    end

    branches = make_sexp_branches(b.ir, b.id, IRTools.branches(b)...)

    let_args = [(x,t) for (x,t) in b if x != undef]
    body = make_lets(let_args, branches)

    sexp("def",name, 
            "Any", nl, tab,
            args, nl,
            body)
end

# Make s-expression for method f
# Generate a top-level function for each basic block,
# Top level: ir is a list of blocks
function make_sexps(ir :: IRTools.IR)
    blocks = IRTools.blocks(ir)

    # Emit blocks n:2 in reverse
    for b in reverse(blocks[2:end])
        se = make_def(false, blockid(b.id), b)
        println(se,"\n")
    end
    # Emit block 1
    f = IRTools.argtypes(blocks[1])[1].val
    se = make_def(true, string(f), blocks[1])
    println(se)
end

###################
### Test make_sexp


print("---\n")
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

function jl2ks(f, argtypes)
    meta = IRTools.typed_meta(Tuple{typeof(f),argtypes...})
    ir = IRTools.IR(meta)
    IRTools.expand!(ir)
    make_sexps(ir)
end

jl2ks(foo1, (Vector{Float64},Float64))

