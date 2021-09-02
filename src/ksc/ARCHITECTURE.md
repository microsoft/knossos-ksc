# The architecture of ksc

`ksc` is a command line executable.  The entry point is `Main.main`.
The most direct way of discovering the command line options, for now,
is to look in the source of `Main`.

## The compilation pipeline

The entry points to the overall compiler pipeline are in
`Ksc.Pipeline`.  See in particular `Ksc.Pipeline.pipeline`.  The
pipeline stages are:

* Parsing: `Ksc.Parse`

  The internal representation into which declarations are parsed is
  defined in `Ksc.Lang`.

* Type checking and inference: `Ksc.Annotate`

* Generation of derived functions: `Ksc.Pipeline.deriveDecl`

  Derived functions are

  * derivatives: `Ksc.AD` and `Ksc.SUF.AD`
  * CatLang functions: `Ksc.CatLang`
  * shape functions: `Ksc.Shapes`

* Optimisation: `Ksc.Opt`

  (including an ANF pass defined in `Ksc.ANF` and optimisation of lets
  in `Ksc.OptLet`)

* Common subexpression elimination: `Ksc.CSE`

* Pruning of unused definitions: `Ksc.Prune`

* C++ codegen: `Ksc.Cgen`

* Futhark codegen: `Ksc.Futhark`

  The Futhark backend is unmaintaned.

The primitive ksc functions are defined in `Ksc.Prim` and the
user-extensible rules system is defined in `Ksc.Rules`.

## Tests

The tests are definied in `Ksc.Test`.
