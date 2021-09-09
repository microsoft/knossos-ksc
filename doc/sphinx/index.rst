.. Knossos documentation master file, created by
   sphinx-quickstart on Thu Feb 11 14:04:47 2021.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

===================
Welcome to Knossos!
===================

.. warning::
  **Knossos is very much a work in progress.**
  Pretty much anything may not work, so we encourage you to 
  say hello at https://github.com/microsoft/knossos-ksc/discussions 
  before even starting to play :)


Knossos compiles (a subset of) PyTorch (and Julia, and F#) code into C++ (and MLIR and ONNX and other stuff).  
By which we mean actual C++, that you can deploy completely runtime-free if you like, 
or linking against ATen, MLAS, ONNX Runtime, whatever you have.

But that's not all -- it also contains a source-to-source automatic differentiation, 
so you can get gradients for free.

The canonical use case is to write custom PyTorch extensions.
Suppose you've invented a great new activation function, which you call ``relu3``:

+------------------------------------------------------------------+-----------------------------------------------------+
+                                                                  +                                                     +
+  .. literalinclude:: ../../examples/dl-activations/relu3.py      + .. figure:: ../../build/doc/relu3-plot.png          +
+      :language: Python                                           +    :scale: 50 %                                     +
+      :start-after: DOC-KS                                        +    :alt: A plot of relu3                            +
+      :end-before: ENDDOC-KS                                      + .. code-block:: python                              +
+                                                                  +                                                     +
+  Defining a new kernel, taking a float and returning a float     +     t = np.arange(-3, 3, step=0.1)                  +
+                                                                  +     plt.plot(t, [relu3(t) for t in t], "b")         +
+------------------------------------------------------------------+-----------------------------------------------------+

It must be better, right?  Smoother than relu, cheaper than gelu.  
So we want to test it out.  Before diving into an MNIST example, 
let's spend a little time looking at the function. 

The natural way to define this elementwise function is to just write the float-to-float 
version as above, but of course we probably want it to work over tensors too.
As in `JAX <https://jax.readthedocs.io/en/latest/notebooks/quickstart.html#auto-vectorization-with-vmap>`_
(or `functorch <https://jax.readthedocs.io/en/latest/notebooks/quickstart.html#auto-vectorization-with-vmap>`_),
we provide ``vmap``, so you can simply write::

   vrelu3 = knossos.vmap(relu3) # Turn float->float into Tensor->Tensor

and then the plotting code above could just use ``vrelu3`` instead of the list comprehension::

  t = torch.arange(-3, 3, step=0.1)
  plt.plot(t, vrelu3(t), "b")

Compilation
-----------

So how is it different to PyTorch or JAX?  Well, as we said above, it's compiled.  
The default option is to compile to C++ source, and one reason for that is simply to 
be able to inspect that source, to reassure ourselves that it's good.  
Here's the C++ for relu3 (slightly prettified, so ``auto c2;`` won't 
compile, but look in ``build/torch_extensions`` to find the real code)

.. literalinclude:: ../../build/doc/relu3.cpp
    :language: C++

So, this is not that different from TorchScript, 
but note that ``ks::Float`` is a real c++ ``float``, 
rather than a wrapper.  And the ``aten::*`` calls are in fact to simple 
inlined C++ functions which you can inspect in the [prelude].
The real difference from other systems comes when we take derivatives.

Derivatives
-----------

Before we use the function for deep learning, let's just examine it a bit more.  
We said it's smooth, meaning, in this case, that its derivative is continuous.
Let's check that  by plotting the derivative.  As ``vrelu3`` takes a 
vector to a vector, its Jacobian is a square matrix.  
And because it's operating elementwise, 
i.e. independently on each element of the vector, the Jacobian is diagonal, 
and a vector-Jacobian product (vjp) with a vector of all ones 
will compute the derivative at each element.
::

  dfdt = vrelu3.vjp(t, torch.ones_like(t)) # Vector-Jacobian product
  plt.plot(t, dfdt, "b")

.. figure:: ../../build/doc/relu3-grad-plot.png
   :scale: 50 %
   :alt: A plot of relu3 and its gradient

So what? I can do that in PyTorch
---------------------------------

Not really, for a few reasons:
 - Knossos compiles your code directly to C++/CUDA.  You can literally look at the output.
 - This means that Knossos can easily deal with control flow like `if` statements, 
   internal function calls, etc.
 - It also means it's efficient for small float->float functions like this.  
   The C++ code really deals in floats, not 1x1 tensors.
 - And the derivative code is also C++, taking plain ol' floats.

So if you try the above example with vmap from functorch or JAX, it just won't work.
Now, if you're an experienced PyTorch programmer, you know that the above is
inefficient and you naturally code it "vectorized".  So you write

.. literalinclude:: ../../examples/dl-activations/relu3.py
     :language: Python
     :start-after: DOC-PTREF
     :end-before: ENDDOC-PTREF   

We argue that while performant, this is not the most natural way to write this code.  
Let's look at the two options side by side:

+------------------------------------------------------------------+--------------------------------------------------------------+
+  .. literalinclude:: ../../examples/dl-activations/relu3.py      + .. literalinclude:: ../../examples/dl-activations/relu3.py   +
+      :language: Python                                           +     :language: Python                                        +
+      :start-after: DOC-KSV                                       +     :start-after: DOC-PTREF                                  +
+      :end-before: ENDDOC-KSV                                     +     :end-before: ENDDOC-PTREF                                +
+                                                                  +                                                              +
+  Knossos: define the kernel, compile with vmap                   + PyTorch: "Thinking in tensors".  It's fun for a while,       +
+                                                                  + but it gets old                                              +
+------------------------------------------------------------------+--------------------------------------------------------------+

The vectorized method may not even be the most efficient.
For example, x^3 is computed for all inputs.  This allows for parallelism 
on massively parallel hardware, but is wasteful on small power-constrained devices.  
Secondly, as written, the computation produces 10 temporary tensors, with a working set 
of up to 3x the optimal computation.


Integrating with PyTorch
------------------------

So, we have a kernel, taking Tensors to Tensors, let's try it in a machine learning model.
We'll make a simple DNN with ``vrelu3`` activations, as in 
`this <https://towardsdatascience.com/extending-pytorch-with-custom-activation-functions-2d8b065ef2fa>`_
tutorial:

.. code:: Python

   # Initialize the model using nn.Sequential
   model = nn.Sequential(OrderedDict([
                ('fc1', nn.Linear(784, 256)),
                ('activation1', vrelu3.nnModule),
                ('fc2', nn.Linear(256, 128)),
                ('bn2', nn.BatchNorm1d(num_features=128)),
                ('activation2', vrelu3.nnModule),
                ('dropout', nn.Dropout(0.3)),
                ('fc3', nn.Linear(128, 64)),
                ('bn3', nn.BatchNorm1d(num_features=64)),
                ('activation3', vrelu3.nnModule),
                ('logits', nn.Linear(64, 10)),
                ('logsoftmax', nn.LogSoftmax(dim=1))]))

   # Run training
   train_model(model)

Yay, it just works.  In this case it's about as fast as the vectorized PyTorch 
(and much much faster than a vmap version), but of course it's easier to write and read,
and easier to modify, and easier to deploy.

Limitations
-----------

  So, it's a general-purpose python compiler! How cool!

No, it's not.  It is intended to allow PyTorch programmers to write more complex models
without falling off performance cliffs.
As mentioned above, a lot of stuff won't work, but here's what we want to get working 
well:

- Float-to-float kernels as above
- Small tensor-to-tensor kernels (see `sqrl <examples/sqrl>`_)
- Messy computer vision models like those in (see `ADBench <examples/sqrl>`_)
- Simple concatenations of operators like LSTM

And we want them all to be as fast as reasonable human-written C++

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`


.. toctree::
   :maxdepth: 2
   :caption: Contents:

   index
   Examples
   SYNTAX
   Benchmarking
   MLIR.md
   About
   Glossary
