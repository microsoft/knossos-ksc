## Knossos Glossary

Concepts that arise throughout the documentation

### KINAUFL: Knossos is not a user-facing language.

The IR as been designed to be ultra-lightweight, in order to simplify interop with a wide range of source languages and backends.  Wherever that light weight causes runtime slowdown, we expect to recover it using rewrite rules.  At the same time, the IR is very strongly designed to be 1-1 with its text representation, to keep compiler passes honest, and to facilitate debugging.  So we tread a tightrope -- it is a merit that it's human readable and writeable, but we want to eschew festoonage.

### OTP: One theorem prover

Greenspun's 10th rule: "Any sufficiently complicated program contains an ad hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp."

And Fitzgibbon's conjecture: "Any sufficiently complicated Common Lisp compiler contains an ad-hoc, informally-specified, bug-ridden, slow implementation of half of a theorem prover."

And Fitzgibbon's related axiom: "Any system should contain at most one theorem prover."

Knossos *is* a theorem prover, so any point at which typechecking or cost computation becomes like half a theorem prover, we aim to re-express it just using the rewrite system.  It's a powerful razor, so when someone invokes "OTP", your mission is to show that your proposed change cannot be done using the existing rewrites.

### SUF: Single use form


An extension of ANF where every variable has exactly one use.  Useful for functional AD and beyond (see https://tminka.github.io/papers/acmll2019/, and https://github.com/microsoft/knossos-ksc/issues/377

### OtOs: Optimized for time or space

Many functions have a choice of space or time efficient implementations, which might be named `foo$Ot` or `foo$Os`.  It's the optimizer's job to choose between them.

### IDTW: If that didn't work

On a list of tasks, describe increasingly desperate alternatives. E.g.

 * Likely option
 * Reasonable option
 * ITDW: now it's fun
 * ITDW: rocket surgery time


### General concepts from related work

 * CSE: Common subexpression elimination
 * DCE: Dead code elimination
 * ANF: A-Normal Form
 * BNF: Backus-Naur Form
