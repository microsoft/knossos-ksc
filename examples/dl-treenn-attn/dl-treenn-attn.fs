type Vec = float[]
type Mat = Vec[]

// learnable parameters for one LSTM unit.
type LSTMWeights = {
  in_dim : int
  hidden_dim : int

  wx : Mat  // learnable parameters for input-to-hidden projection with the shape [hidden_dim x 4, in_dim]
  wh : Mat  // learnable parameters for hidden-to-hidden projection with the shape [hidden_dim x 4, in_dim]
  b : Vec   // learnable bias with shape [1, hidden_dim x 4]
end

// learnable parameters for the attention module.
struct AttentionWeights
  enc_proj : Mat
  dec_hidden_proj : Mat
  attn_proj : Mat  // this projection projects hidden vectors into unnormalized attention scores.
end

// all learnable parameters for the seq2seq model.
struct Seq2SeqWeights
  src_embedding : Mat
  trg_embedding : Mat
  encoder::Array{LSTMWeights}  // parameters for bi-directional LSTM
  decoder::LSTMWeights
  attn::AttentionWeights
  pre_softmax_proj : Mat
end
sigmoid, tanh and softmax activations.

σ(x) = 1 / (1 + exp(-x))

tanh(x) = (exp(x) - exp(-x)) / (exp(x) + exp(-x))

function softmax(m : Mat)
  maxval = maximum(m, 2)
  out .= exp.(m .- maxval)
  out ./= sum(out, 2)
end
Embedding layer

// This overloading function defines the embedding layer
// which is a lookup table in essence.
A : Mat * B::Array{Int} = A[:, B]
LSTM and bi-directional LSTM

offset(x:Mat, h : int, n : int) = x[(n - 1) * h : h * n, :]

function LSTMUnit(h : Mat, c : Mat, x : Mat, m::LSTMWeights)
  proj = m.Wi * x .+ m.Wh * h .+ m.b
  input = σ.(offset(proj, m.hidden_size, 1))
  forget = σ.(offset(proj, m.hidden_size, 2))
  cell = tanh.(offset(proj, m.hidden_size, 3))
  output = σ.(offset(proj, m.hidden_size, 4))
  c = forget .* c .+ input .* cell
  h′ = output .* tanh.(c)
  return h′, c
end

// inputs is a matrix with shape [in_dim x total_token_number].
// This function iterate over the input sequence.
function LSTM(inputs : Mat, seq_len : int, hidden_dim : int;
              m::LSTMWeights; reverse=false)
  token_size = size(inputs, 1)
  batch_size = size(inputs, 2) / seq_len  // sequence number in a mini-batch

  // intialize the output buffer
  cells = zeros(hidden_dim, token_size)
  hiddens = zeros(hidden_dim, token_size)

  h, c = zeros(hidden_dim, batch_size), zeros(hidden_dim, batch_size)
  for i = 1 : seq_len
    start_pos = reverse ? (seq - i) * batch_size : (i - 1) * batch_size
    end_pos = start_pos + (reverse ? -batch_size : batch_size)
    
    // recurrent computation.
    h, c = LSTMUnit(h, c, inputs[:, start_pos:end_pos], m)

    cells[:, start_pos : end_pos] = c
    hiddens[:, start_pos : end_pos] = h
  end
  return hiddens
end

// bi-directional LSTM encoder
function biLSTM(src_embedding : Mat,
                seq_len : int, in_dim : int, hidden_dim : int,
                m::Array{LSTMWeights})
  fwd = LSTM(src_embedding, seq_len, hidden_dim, m[1])
  bwd = LSTM(src_embedding, seq_len, hidden_dim, m[2]; reverse=true)
  hcat(fwd, bwd)
end
The attention module

function attention(batch_size : int,
                   encoder_outputs : Mat, cur_input : Mat, h : Mat,
                   a::AttentionWeights)
  // Attention is a fully connected feed-forward network.
  // This function implements an additive attention.
  src_len = size(encoder_outputs, 2) / batch_size

  // project encoder outputs into a new hidden space.
  enc_proj = encoder_outputs * a.enc_proj

  // calculate unnormalized attention scores.
  vec = enc_proj + repeat(h * a.dec_hidden_proj, src_len)
  attn_scores = vec * a.attn_proj // project into 

  // normalize into a distribution,
  attn_weights = softmax(reshape(attn_scores, (src_len, batch_size)))
  // sum pooling
  context_vec = sum(attn_weights .* encoder_outputs)
end
LSTM decoder with attention

function decoder(trg_embedding : Mat, encoder_outputs : Mat,
                 hidden_dim : int, trg_len : int,
                 attn_w::AttentionWeights, m::LSTMCell)
  token_size = size(trg_embedding,
  batch_size = size(trg_embedding, 2) / trg_len

  cells = zeros(hidden_dim, token_size)
  hiddens = zeros(hidden_dim, token_size)
  
  // intial states
  h = zeros(hidden_dim, batch_size)
  c = zeros(hidden_dim, batch_size)

  // Recurrent computation over the target sequence batch.
  for i = 1 : trg_len
    // The computation in one time step is customizable.
    // Modify the loop body as needed.
    start_pos = batch_size * (i - 1)
    end_pos = start_pos + batch_size 

    cur_input = trg_embedding[:, start_pos : end_pos]
    ctx_vector = attention(batch_size, encoder_outputs, cur_input, h, attn_w)
    h, c = LSTMUnit(h, c, cur_input + ctx_vector, m)

    cells[:, start_pos : end_pos] = c
    hiddens[:, start_pos : end_pos] = h
  end
  return hiddens
end
Hyperparameters and Generate random experimental data.

const vocab_dim = 10000 // There is 10000 words in the vocabulary.
const batch_size = 16
const src_len = 50
const trg_len = 80

const in_dim = 64
const hidden_dim = 64

// generate random inputs sequence which is in one-hot representation.
function gen_batch()
  src = rand(1 : vocab_dim, batch_size * src_len)
  trg = rand(1 : vocab_dim, batch_size * trg_len)
  trg_cur = trg[1 : end - 1]
  trg_next = trg[2 : end] // the groundtruth
  return (src, trg_cur, trg_next)
end
Train the model.

function seq2seq(src, trg, weights)
  src_emb = src * weights.src_embedding
  enc_outputs = biLSTM(src_emb, seq_len, in_dim, hidden_dim,
                       weights.encoder)

  trg_emb = trg * weights.trg_embedding
  dec_outputs = decoder(trg_emb, enc_outputs, hidden_dim, trg_len,
                        weights.attn_weights, weights.decoder)

  prob = softmax(dec_outputs * weights.pre_softmax_proj)
end

function loss(src, trg, trg_next, weights)
  sum(crossentropy(seq2seq(src, trg, weights), trg_next))
end

// initialize learnable weights
weights = WeightsInit(vocab_dim, in_dim, hidden_dim)

// Run SGD algorithm for 100 mini-batches to optimize the seq2seq model.
// sgd will invoke the AD interface.
@epochs 100 sgd(w -> loss(src, trg, trg_next, w), gen_batch(), weights)