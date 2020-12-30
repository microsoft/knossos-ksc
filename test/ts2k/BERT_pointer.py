import sys
import torch
from transformers import BertConfig, BertScriptableForQuestionAnswering

from ts2ks import ts2ks, ts2ks_fromgraph

#configuration = BertConfig(num_hidden_layers = 1)
config = BertConfig(num_hidden_layers = 1)
model = BertScriptableForQuestionAnswering(config)

# We can't currently use this approach as much of the BERT initialisation code is NOT TorchScript compatible
@torch.jit.script
def main():
    # Initializing a model from the bert-base-uncased style configuration
    #model = BertForQuestionAnswering(configuration)
    #model = BertScriptableForQuestionAnswering(config)
    pass


# Need a no-init config?

config = BertConfig(num_hidden_layers = 1)
model = BertScriptableForQuestionAnswering(config)

scripted = torch.jit.script(model)

#print(scripted.graph)
#print(scripted.inlined_graph)

generate_edef = False

ts2ks_fromgraph(sys.stdout, generate_edef, "BertScriptableForQuestionAnswering", scripted.inlined_graph)

# scripted_init_weights = torch.jit.script(model.init_weights)
# scripted_init_weights.graph