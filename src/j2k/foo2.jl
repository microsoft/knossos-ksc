f(x) = cos(x) * x
sumsq(xs) = sum(xs.^2)

function foo2(as :: Vector{Float64}, b :: Float64)
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