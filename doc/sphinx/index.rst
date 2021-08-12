.. Knossos documentation master file, created by
   sphinx-quickstart on Thu Feb 11 14:04:47 2021.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.


.. toctree::
   :maxdepth: 2
   :caption: Contents:

   About
   SYNTAX
   relu3
   Glossary


Welcome to Knossos!
===================

Knossos turns (a subset of) PyTorch (and Julia, and F#) code into C++ (and MLIR and ONNX and other stuff).  
By which we mean actual C++, that you can deploy completely runtime-free if you like, 
or linking against ATen, MLAS, ONNX Runtime, whatever you have.

But that's not all -- it also contains a source-to-source autodiff, so you can get gradients for free.

The canonical use case is to write custom PyTorch extensions.
Suppose you've invented a great new activation function, which you call ``relu3``:

.. literalinclude:: ../../examples/dl-activations/relu3.py
     :language: Python
     :start-after: DOC-KS
     :end-before: ENDDOC-KS

It must be better, right?  Smoother than relu, cheaper than gelu sounds like an excellent idea.  
Well, let's try it in an MNIST model.  First, however, we should make it work on tensors:

.. code:: Python

   vrelu3 = torch.vmap(relu3)

Like in JAX, this takes a function defined on scalars, and "vectorizes" it to work on
Tensors.  OK, it doesn't actually work in PyTorch 1.9, because the `if` statement can't 
be translated, but it could.  Even if it did work, you would probably expect it to be 
slow.  Those with pytorch experience will know that for speed, one needs to write code 
that is vectorized from the start, something like this.

.. literalinclude:: ../../examples/dl-activations/relu3.py
     :language: Python
     :start-after: DOC-PTREF
     :end-before: ENDDOC-PTREF   

With Knossos, we argue that this is neither the most natural way to program, nor even 
the fastest.  For example, x^3 is computed for all inputs.  This allows for parallelism 
on massively parallel hardware, but is wasteful on small power-constrained devices.  
Secondly, as written, the computation produces 10 temporary tensors, with a working set 
of up to 3x the optimal computation.


Now if you know anything about PyTorch, you should be suspicious now: isn't this going 
to be glacially slow?  Well, let's see.  
We'll make a simple DNN with ``vrelu3`` activations, as in [](https://towardsdatascience.com/extending-pytorch-with-custom-activation-functions-2d8b065ef2fa)

.. code:: Python

   # Initialize the model using nn.Sequential
   model = nn.Sequential(OrderedDict([
                        ('fc1', nn.Linear(784, 256)),
                        ('activation1', vrelu3),
                        ('fc2', nn.Linear(256, 128)),
                        ('bn2', nn.BatchNorm1d(num_features=128)),
                        ('activation2', vrelu3),
                        ('dropout', nn.Dropout(0.3)),
                        ('fc3', nn.Linear(128, 64)),
                        ('bn3', nn.BatchNorm1d(num_features=64)),
                        ('activation3', vrelu3),
                        ('logits', nn.Linear(64, 10)),
                        ('logsoftmax', nn.LogSoftmax(dim=1))]))

   # Run training
   train_model(model)

And after a few hours, you can hit control C.  It was really slow.  

Compiling with Knossos
----------------------

This is where Knossos comes to the rescue.

.. code:: Python

   vrelu3 = knossos.vmap(vrelu3)

And run again, amazingly fast!

So what's happening?


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
