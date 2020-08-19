using Distributions: Gaussian, Uniform
using Statistics: std, mean, stdm
using Random: AbstractRNG, MersenneTwister

# Utils
const Vec = Array{Float64, 1}
const Mat = Array{Float64, 2}
const Ten3 = Array{Float64, 3}
const Ten4 = Array{Float64, 4}

randg(dims; μ=0.1, σ=1e-3) = rand(Gaussian(μ, σ), dims...);

function softmax(x :: Vec) 
    # From https://en.wikipedia.org/wiki/Softmax_function
    ex = exp.(x)
    ex ./ sum(ex)
end

drop(x;p = 0.1) = (x < p) ? 0.0 : 1.0
relu(x) = max(x, 0.0)
relu(x :: Vec) = relu.(x)

########################################################################
# Base type for "layers" is just a placeholder for any function-like object
# There will typically be initializer code and "Apply" code

abstract type Op end

# Apply 
# Default implementation just errors -- it should have been implemented in the derived class
# Actually not too helpful, as supresses useful error message at call site
# (self::Op)(x :: Any) = error("unimplemented") 

########################################################################
struct Linear <: Op
    W::Mat
    b::Vec
end

# Initialize
Linear(n_in, n_out = n_in, μ=0.1, σ=1e-2) = Linear(randg((n_out, n_in); μ=μ, σ=σ), zeros(n_out))

# Apply
(self::Linear)(x::Vec) = self.W * x .+ self.b

# Test/example
function test_Linear()
    vecs = [rand(2) for i in 0:3]
    Linear(2,3).(vecs)  # Broadcast over the vecs
end

########################################################################
struct Relu <: Op
end

# Apply
(self::Relu)(x::Vec) = relu.(x)

########################################################################
struct LayerNorm <: Op
    a :: Vec
    b :: Vec
    eps :: Float64
end

# Initialize
LayerNorm(n :: Int, eps = 1e-6) = LayerNorm(ones(n), zeros(n), eps)

# Apply
function (self::LayerNorm)(x :: Vec)
    m = mean(x)
    s = stdm(x, m) 
    return self.a .* (x .- m) ./ (s + self.eps) + self.b
end

# Test/example
function test_LayerNorm()
    LayerNorm(3)(rand(3))
end

########################################################################
struct Dropout <: Op
    rng :: AbstractRNG
    rate :: Float64
end

# Apply
(self::Dropout)(x :: AbstractArray) = x .* (rand(self.rng, size(x)...) .> self.rate)

# Test/example
function test_Dropout()
    sum(Dropout(MersenneTwister(123), 0.0)(rand(1000)) .> 0)
end
test_Dropout()

########################################################################
# Translation directly from "The Annotated Transformer"
# http://nlp.seas.harvard.edu/2018/04/03/attention.html
# Defined bottom up, a style common in strongly typed languages
# 
# SublayerConnection
# MultiHeadedAttention
# PositionwiseFFN
# Embeddings
# PositionalEncoding
# EncoderLayer
# DecoderLayer
# model
#   EncoderDecoder(
#    Encoder(EncoderLayer(d_model, MultiHeadedAttention, PositionwiseFFN, dropout), N),
#    Decoder(DecoderLayer(d_model, MultiHeadedAttention, MultiHeadedAttention, PositionwiseFFN, dropout), N),
#    Embeddings(d_model, src_vocab) ∘ PositionalEncoding,
#    Embeddings(d_model, tgt_vocab) ∘ PositionalEncoding),
#    Generator(d_model, tgt_vocab))

########################################################################
struct Sublayer <: Op
    dropout :: Dropout
    op :: Op # Wrapped op
    norm :: LayerNorm
end

(self::Sublayer)(x :: Vec) = x + self.dropout(self.op(self.norm(x)))

########################################################################

# Attention is just a function, not an Op as such
function attention(Q :: Mat, K :: Mat, V :: Mat, mask :: BitArray{2}, dropout)
    # Q: n x d      n queries, one per row, each dimension d
    # K: m x d      m keys, one per row, each dimension d
    # V: m x p      m values, one per row, each dimension q
    # mask: n x m   is query i allowed to look at key j
    n,dk = size(Q)

    return vcat([ 
        # Loop through the rows of Q -- more natural, and a step towards variable length input, and avoiding the upper triangular mask hack
        dropout(softmax(K[valid,:] * q / √(dk)))' * V[valid,:]
        for (q,valid) in zip(eachrow(Q),eachrow(mask))
    ]...)
end

# Test/example
function test_attention()
    n = 3
    m = 7
    d = 17
    p = 23
    σ = 10.0

    Q = randg((n,d),σ=σ)
    K = randg((m,d),σ=σ)
    V = randg((m,p),σ=σ)
    Mask = randg((n,m)) .> 0
    attention(Q,K,V,Mask, identity)
end
test_attention()

########################################################################
struct MultiHeadedAttention <: Op
    WQ :: Array{Linear}   # The webpage says "Linear", paper says Matrix, let's do linear as easier
    WK :: Array{Linear} 
    WV :: Array{Linear}
    WO :: Linear
    dropout :: Dropout
end

function (self::MultiHeadedAttention)(Q :: Mat, K :: Mat, V :: Mat, mask :: BitArray{2})
    h = size(self.WQ)
    return hcat([
        attention(self.WQ[i](Q), self.WK[i](K), self.WV[i](V), mask, self.dropout) 
        for i = 1:h
    ]...) |> WO 
end

########################################################################
struct PositionwiseFFN <: Op
    L1 :: Linear
    L2 :: Linear
    dropout :: Dropout
end

(self::PositionwiseFFN)(x :: Vec) = x |> self.L1 |> relu |> self.dropout |> self.L2 

function test_PositionwiseFFN()
    PositionwiseFFN(Linear(2,3), Linear(3,4), Dropout(MersenneTwister(121), 0.1))(rand(2))
end
test_PositionwiseFFN()

########################################################################
struct Embeddings <: Op
    lut :: Array{Vec}
end

Embeddings(N :: Int, dim :: Int; sigma=1.0) = Embeddings([randg((dim);σ=sigma) for _ = 1:N])

(self::Embeddings)(xs :: Array{Int}) = [self.lut[x] / √(size(self.lut[1],1)) for x in xs]

function test_Embeddings()
    Embeddings(10,3)([rand(1:10) for _ = 1:7])
end
test_Embeddings()

########################################################################
struct PositionalEncoding <: Op
    dropout :: Dropout
    d_model :: Int
end

function (self::PositionalEncoding)(xs :: Array{Vec})
    # Compute the positional encodings once in log space.
    len = length(xs)
    positions = 0:len-1
    # need this for bit-for-bit equality to python:
    # divs = exp.(range(0.0,-log(10000); length=(d_model ÷ 2)+1))[1:end-1]
    divs = exp.(range(0,-log(10000); length=self.d_model ÷ 2))

    pe = [vec([sin.(pos./divs) cos.(pos./divs)]') for pos in positions]

    return self.dropout(xs .+ pe)
end

function test_PositionalEncoding()
    V = 11
    len = 113
    seq = [rand(1:V) for _ = 1:len]
    d_model = 16
    x = Embeddings(V,d_model,sigma=0.0)(seq)
    PositionalEncoding(Dropout(MersenneTwister(125), 0.0), d_model)(x)
end
test_PositionalEncoding()

