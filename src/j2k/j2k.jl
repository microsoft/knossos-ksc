# Julia to Knossos
using Zygote
using IRTools

f(x) = cos(x) * x


function foo1(a,b)
    if a > 0
        y = sin(a) * f(b)
    else
        y = -a*f(b)
    end
    f(y) + 5
end

ct = code_typed(foo1, typeof((1.1,2.2)); optimize=false)                  

#ct = code_typed(Zygote.gradient, typeof((foo1,1.1,2.2)); optimize=true)                     
@assert length(ct) == 1
codeinfo,type = ct[1]

show(codeinfo)

function sp(x)
    print("[unknown ", typeof(x), "]")
end

function sp(x::Number)
    print(x)
end

function sp(x::Core.SSAValue)
    print(x)
end

function sp(x::Core.SlotNumber)
    print(x)
end

function sp(x::GlobalRef)
    if x.mod != Base
        print(x.mod,"::")
    end
    print(x.name)
end

function sp(x::Core.GotoNode)
    print("goto ", x.label)
end

function sp(x::Expr)
    if x.head == :invoke
        print("(I ", x.args[1].def.name)
        for c in x.args[2:end]
            print(" ")
            sp(c)
        end
        print(")")

    elseif x.head == :call
        print("(C ", x.args[1])
        for c in x.args[2:end]
            print(" ")
            sp(c)
        end
        print(")")

    elseif x.head == :return
        print("(return", )
        for c in x.args
            print(" ")
            sp(c)
        end
        print(")")
    else
        print(x.head, " -- ", x)
    end
end

print("---\n")
for i=1:length(codeinfo.code)
    print("%", i, " = ")
    sp(codeinfo.code[i])
    print("\n")
end
