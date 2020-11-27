import torch
from transformers import BertConfig, BertScriptableForQuestionAnswering

#configuration = BertConfig(num_hidden_layers = 1)
config = BertConfig(num_hidden_layers = 1)
model = BertScriptableForQuestionAnswering(config)

@torch.jit.script
def main():
    # Initializing a model from the bert-base-uncased style configuration
    #model = BertForQuestionAnswering(configuration)
    #model = BertScriptableForQuestionAnswering(config)


    print("Hello BERT from TorchScript -> Knossos!")


# Need a no-init config?

config = BertConfig(num_hidden_layers = 1)
model = BertScriptableForQuestionAnswering(config)

scripted = torch.jit.script(model)

#print(scripted.graph)

print(scripted.inlined_graph)

#print(main.graph)

# scripted_init_weights = torch.jit.script(model.init_weights)
# scripted_init_weights.graph