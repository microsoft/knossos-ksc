
# optimize transformer-based models with onnxruntime-tools
from onnxruntime_tools import optimizer
from onnxruntime_tools.transformers.onnx_model_bert import BertOptimizationOptions

from transformers.convert_graph_to_onnx import load_graph_from_args,convert_pytorch
from pathlib import Path

if False:
    print("hf-bert-to-onnx: Loading graph")
    nlp = load_graph_from_args("feature-extraction", "pt", "bert-base-cased")
    #!mkdir -p obj/bert-base-cased
    print("hf-bert-to-onnx: Saving onnx")
    convert_pytorch(nlp, opset=11, output=Path("obj/bert-base-cased/bert.onnx"), use_external_format=True)
    #!ls -l obj/bert-base-cased

    print("hf-bert-to-onnx: Optimizing")
    # disable embedding layer norm optimization for better model size reduction
    opt_options = BertOptimizationOptions('bert')
    opt_options.enable_embed_layer_norm = False

    h = 3
    opt_model = optimizer.optimize_model(
        'obj/bert-base-cased/bert.onnx',
        'bert', 
        num_heads=h,
        hidden_size=768,
        optimization_options=opt_options)
    opt_model.save_model_to_file(f'obj/bert-base-cased/bert.opt.{h}.onnx')


from transformers import BertModel, BertForQuestionAnswering, BertConfig, BertTokenizer, pipeline

tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')

for d in range(1,24):
    # Initializing a BERT bert-base-uncased style configuration
    configuration = BertConfig(num_hidden_layers = d)
    # Initializing a model from the bert-base-uncased style configuration
    model = BertForQuestionAnswering(configuration)

    p = pipeline('question-answering',model=model,tokenizer=tokenizer)

    dest = Path("obj/hf-bert-to-onnx")
    dest.mkdir(parents=True,exist_ok=True)

    print("*** ", d)
    print("hf-bert-to-onnx: Saving onnx")
    convert_pytorch(p, opset=11, output=dest / f'bert{d}.onnx', use_external_format=True)
