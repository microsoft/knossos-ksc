# Knossos plan of work

This document is a high-level overview of the plan of work for Knossos
over the coming months.  It consists of a description of Knossos, a
description of three milestones, and more detail about the progression
of the features that determine the milestones.

## Description of Knossos

The Knossos compiler will take an input program written in a source
language (which supports a particular set of source language
constructs) and transform it to an intermediate representation (IR)
which is the form to which it applies its compiling and optimising
transformations.  Certain rewrite rules will be applied by a rewrite
engine with the aim of optimising the program.  The output of the
compiler will be a program in a target language which will
subsequently be compiled by an off-the-shelf compiler, but not one
that needs to provide fancy optimisation passes.

More detail on each of the individual aspects is given in the section
on feature progression below.

The Knossos compiler is aimed at numerical code, particularly code
that implements numerical optimisation routines.

The aim of the Knossos compiler is to provide a foundation for
investigating the use of machine learning to guide program
optimisation through local rewrite rules.  As well as the most
familiar rewrite rules from the field of optimising compilers, the
Knossos compiler is expected to include rewrite rules for automatic
differentiation and destination passing style.

## Milestones

I envisage three key milestones for the Knossos compiler over the
coming months.  The first is the minimum functional implementation,
i.e. a compiler that can actually compile a useful program.  The
second is the minimal viable product, i.e. a compiler that could
actually be used in practice for compiling source code to reasonably
efficient output.  The third is a compiler that applies sophisticated
rewrite rules guided by machine-learning to produce highly optimised
code competitive with existing hand written code.

Each milestone is specified by the progress of each feature at that
milestone.  See the section below for more details on the progression
of features.

All the milestones require that the IR is implemented.

### Milestone 1 (minimum functional implementation)

* Input programs: GMM
* Source language: embedded IR
* IR concrete syntax: not defined
* Rewrite rules: with Haskell/F# pattern matching
* Rewrite engine: none
* Target language: interpreter

### Milestone 2 (minimum viable product)

* Input programs: GMM, BA, HT, LSTM
* Source language: F#
* IR concrete syntax: defined
* Rewrite rules: with Haskell/F# pattern matching
* Rewrite engine: hand coded
* Target language: `.c`

### Milestone 3 (end of calendar year 2018)

* Input programs: GMM, BA, HT, LSTM
* Source language: F#
* IR concrete syntax: defined
* Rewrite rules: with Haskell/F# pattern matching
* Rewrite engine: machine learned
* Target language: `.c` in destination passing style

## Feature progression

This section provides an overview of how each feature of the compiler
is anticipated to develop.  The stages of development for each feature
are listed in increasing order of difficulty, which is the same as the
proposed order in which they are to be accomplished.

* Input programs

  Implementations of the following in "Source language" can be
  successfully differentiated, compiled and run

  1. Gaussian mixture model
  2. GMM, bundle adjustment
  3. GMM, BA, hand tracking
  4. GMM, BA, HT, LSTM

* Source language

  The source language is

  1. the embedded IR
  2. a well defined concrete syntax for the IR (`.ks`, "Knossos file") (see below)
  3. Subset of Haskell (no easier than subset of F# but less desirable
  so most likely will be skipped)
  4. F# (or rather a subset called "[F
  smooth](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/dps-fhpc17.pdf)")

* Source language constructs

  All of

  * let
  * apply
  * recursion
  * top level function definitions
  * lambda
  * array map

* IR concrete syntax

  The concrete syntax format of the intermediate representation,
  i.e. the concrete syntax of `.ks` source files, is

  1. not defined
  2. well defined

* IR

  The Knossos intermediate representation, on which we will do all our
  program transformations, is

  1. not defined
  2. defined
  3. implemented

* Rewrite rules

  Rewrite rules are implemented

  1. with Haskell/F# pattern matching, i.e. embedded into the compiler
  2. in special separate source input file at compiler *compile* time
  3. in special separate source input file at compiler *run* time
  4. in the same source input file as the code

* Rewrite engine

  1. none
  2. hand coded
  3. machine learned

* Target language

  The result of the Knossos compiler will be to

  1. run programs in in-Haskell (or -F#) interpreter
  2. generate `.hs`/`.fs` files
  3. generate LLVM IR (harder than generating `.c` but less desirable
  so most likely will be skipped)
  4. generate `.c` files
  5. generate `.c` files in [destination passing
  style](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/dps-fhpc17.pdf)
