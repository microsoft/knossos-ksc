# Julia to Knossos
using Zygote
using IRTools
import Base.show

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

struct Ex 
    args :: Array{Any}
end

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
    string(x.mod, "\$", x.name)
end

function make_sexp(x :: Expr)
    if x.head == :call
        sexp(map(make_sexp, x.args)...)
    else
        sexp("[EXPR<", x.head, ">:", x, "]")
    end
end

function make_sexp(x :: IRTools.Variable, st :: IRTools.Statement) 
    if isnothing(st.expr)
        ex = sexp("!")
    else
        ex = make_sexp(st.expr)
    end
    if isnothing(x)
        sexp("nil", ex)
    else
        sexp("let",string("v%", x.id),ex)
    end
end

# struct IRTools.Branch
#     condition :: Any
#     block     :: Int64
#     args      :: Array{Any,1}

function make_sexp_br(id :: Int, br :: IRTools.Branch)
    if br.block == 0
        sexp(br.args...)
    else
        sexp(string("%b", br.block), br.args...)
    end
end

function make_sexp_brs(id :: Int, br :: IRTools.Branch)
    if isnothing(br.condition)
        make_sexp_br(id, br)
    else
        sexp("if", br.condition, sexp(string("b",id+1)), make_sexp_br(br))
    end
end

function make_sexp_brs(id :: Int, br :: IRTools.Branch, tail...)
    # Must be conditional if tail nonempty
    @assert !isnothing(br.condition)
    sexp("if", br.condition, make_sexp_br(id, br), make_sexp_brs(id, tail...))
end

function make_sexp(b :: IRTools.Block)
    ats = zip(IRTools.arguments(b),IRTools.argtypes(b))
    args = sexp([sexp(a,":",t) for (a,t) in ats]...)

    if false
        for (x,t) in b
            if x == undef
                println("T>>>", t)
            else
                println(x," = ", t)
            end
        end
    end

    exs = [make_sexp(x,t) for (x,t) in b if x != undef]
    body = sexp(exs...)
    last = make_sexp_brs(b.id,IRTools.branches(b)...)
            
    sexp("def",string("b",b.id), 
         "Any",
         args,
         body,
         last
         )
end

print("---\n")
ir
for b in reverse(IRTools.blocks(ir))
    println(make_sexp(b))
end
