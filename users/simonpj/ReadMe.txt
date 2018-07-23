This is a simple automatic differentiator

Modules
~~~~~~~
* Lang: the data type for the language, and its pretty-printer
  (No parser yet.)

* Prim: helper functions to build calls to primitives

* Opt: An optimiser for the languag

* AD: Automatic differentiator

  - gradD :: Def -> Def
    transforms a function definition for f into
    a function definition that builds the full Jacobian

  - transposeD :: Def -> Def
    transforms a function that returns the full Jacobian
    into one that returns the /transposed/ Jacobian

  - applyD :: Def -> Def
    applies the Jacobian to an argument

* Main: Run tests


Running it
~~~~~~~~~~
Do 'ghci Main', and then try
   demo ex1
   demo ex2
   demo ex3

