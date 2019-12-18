import Random
rng0 = Random.MersenneTwister(1234);

const Mat = Matrix{Float64}
const Vec = Vector{Float64}

struct RNN
    Wx :: Mat  # Transform input to hidden
    Wh :: Mat  # Transform hidden to hidden
    b  :: Vec  # Bias hidden to hidden
end

# RNN model: Given input sequence xs, compute output sequence
function rnn(weights :: RNN, xs :: Array{Vec}) :: Array{Vec}
    h = rand(rng, length(weights.b))
    [
      h = tanh.(weights.Wh * h) + tanh.(weights.Wx * x) + weights.b
         for x in xs 
    ]
end

# RNN model: Given input sequence xs, compute output sequence
function rnn2(weights :: RNN, xs :: Array{Vec})
    h = rand(rng, length(weights.b))
    show(h)
    out = []
    for x in xs
        h = tanh.(weights.Wh * h) + tanh.(weights.Wx * x) + weights.b
        push!(out, h)
    end
    out
end

weights = RNN(rand(5,3), rand(5,5), rand(5))

xs = [rand(rng, 3) for i in 1:5];

rng = copy(rng0);
r1 = rnn(weights, xs)

rng = copy(rng0);
r2 = rnn2(weights, xs)
