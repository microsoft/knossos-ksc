# Zygote on higher-order programs
const Float = Float64

function make_f(a :: Float)
    b = 2a
    t -> sin(b*t)
end

function g(a, x)
    f = make_f(a)
    f(cos(x))
end

f(a :: Float, t :: Float) = sin(2a*t)
g1(a, x) = f(a,cos(x))
g2(a, x) = sin(2a*cos(x))

println(g(3.0,7.0), " =? ", g1(3.0,7.0))

using Zygote
println(Zygote.gradient(g2, 3.0, 7.0))
println(Zygote.gradient(g1, 3.0, 7.0))
println(Zygote.gradient(g, 3.0, 7.0))

using IRTools
Zygote.@code_adjoint make_f(3.0)


#------------------------------------------

function h(as, xs)
    fs = map(make_f, as)
    vals = map(map, fs, map(cos, xs))
    sum(vals)
end

as = [x + 0.1 for x in 1:4]
xs = [sqrt(x) for x in 1:4]

map(make_f, as)

println(h(as, xs))
println(Zygote.gradient(h, as, xs))

