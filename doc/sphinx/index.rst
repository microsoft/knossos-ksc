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
Well, let's try it in an MNIST model.  First, let's define our mnist model.




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
