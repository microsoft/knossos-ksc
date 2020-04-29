import torch
import sexpdata


# Newline constant for s-expr printing
nl = "\n"
tab = "\t"

# @torch.jit.script
# def sqr(x):
#      torch.mul(x,x)

# @torch.jit.script
# def g(x,y):
#     return sqr(torch.sin(x*y) if x > 0 else -y*torch.sin(-x))

@torch.jit.script
def f(x):
    return x + 4.0

@torch.jit.script
def main():
    print("Hello world from TorchScript -> Knossos!")

#https://github.com/pytorch/pytorch/blob/master/torch/csrc/jit/OVERVIEW.md
#https://pytorch.org/docs/master/jit.html#interpreting-graphs
#https://github.com/pytorch/pytorch/blob/8fe2a5e91b79e3f9b6f6c632fdf7f39ec3bf4fca/torch/csrc/jit/ir/ir.h

print("#| -------- Graph ----------")
print(f.graph)
print("-------- |#")


# (def foo1 Any 
#  	 ((%2 : Array{Float64,1}) (%3 : Float64)) 
#  (let (_4 (Base.broadcasted Main.__anon__.* _2 4.0)) 
#  (let (_5 (Base.materialize _4)) 
#  (%5))))

# print(type(f)) 

# print(f.code) 

symbolLook = {
    "Tensor": [sexpdata.Symbol("Vec"), sexpdata.Symbol("Float")]# float vs integer?
}


def make_arg(input):
    #[input.debugName(), " :", input.type()]
    return [
        sexpdata.Symbol(input.debugName()),
        sexpdata.Symbol(":"),
        symbolLook[str(input.type())]
    ]

def ts2ks(function):

    name = sexpdata.Symbol("main") #sexpdata.Symbol(function.name)

    [make_arg(item) for item in function.graph.inputs() ]
    args = list(map(make_arg, function.graph.inputs()))
    body = [sexpdata.Symbol("print"), "\n" "Hello world from TorchScript -> Knossos!" "\n"]
    wholeExp = [sexpdata.Symbol('def'), name, sexpdata.Symbol("Integer"), args, body]

    function.graph.inputs()

    # for node in function.graph.nodes():

    #     name = function.name

    #     print(node)

    #     print(node.kind())

    #     for inpu in node.inputs():
    #         print(inpu)

    print(sexpdata.dumps(wholeExp))

ts2ks(main)


#print(dir(e.graph.nodes))
#print(e.code)


# print(f.graph)
# print(f.code)