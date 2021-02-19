.. Knossos documentation master file, created by
   sphinx-quickstart on Thu Feb 11 14:04:47 2021.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Knossos!
===================

Knossos turns (a subset of) PyTorch code into C++ (and, one day, MLIR and ONNX and other stuff).  
By which we mean actual C++, that you can deploy completely runtime-free if you like, 
or linking against ATen, MLAS, ONNX Runtime, whatever you have.

But that's not all -- it also contains a source-to-source autodiff, so you can get gradients for free.

The canonical use case is to write custom PyTorch extenstions.
Suppose you've invented this great new activation function ``relu3``:

.. literalinclude:: ../../examples/dl-relu3/relu3.py
     :language: Python
     :start-after: BEGINDOC
     :end-before: ENDDOC


It should be better, right?  Smoother with a robust backbone sounds like just the ticket for better convergence.  
Well, let's try it in an MNIST model.  First, however, we should make it work on tensors:

.. code:: Python

   from knossos import vmap
   def vrelu3(t : Tensor) -> Tensor:
      return vmap(relu3, t)

Or even just ``vrelu3 = vmap(relu3)``.  Like in JAX, this takes a function defined on scalars, 
and "vectorizes" it to work on Tensors (or in fact any container of scalars, such as a tuple of tensors of tuples).

Now if you know anything about PyTorch, you should be suspicious now: is'nt this going to be glacially slow?
Well, let's see.  We'll make a simple DNN with ``vrelu3`` activtations, as in [](https://towardsdatascience.com/extending-pytorch-with-custom-activation-functions-2d8b065ef2fa)

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

Knossos.compile
---------------

This is where Knossos comes to the rescue.

.. code:: Python

   vrelu3 = knossos.compile(vrelu3)

And run again, amazingly fast!

So what's happening?



.. toctree::
   :maxdepth: 2
   :caption: Contents:

   About
   SYNTAX
   relu3

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
