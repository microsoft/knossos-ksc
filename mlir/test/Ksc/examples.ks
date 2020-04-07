; Knossos tests, should be parseable into MLIR and LLVM.
; However, there are a number of tests that do not pass for various reasons.
; Here are the ones that do pass. If new ones are supported, add them here.

; For the inlined tests here:
; RUN: ksc-mlir LLVM %s 2> /dev/null

; Unsupported tests need:

; adbench-lstm.ks
;  * Vector of Tuples of Vectors
;  * constVec
; derivative-selection.ks:
;  * $inline
; edef.ks:
;  * LM type
; fold.ks:
;  * Tuple of Vectors
;  * $check, $ranhashdoub
;  * Auto-derivation (rev$, fwd$)
; generated_deltavecs.ks:
;  * sumbuild, assert
; gmm.ks:
;  * Vector of Vectors, Tuple of Vectors
;  * assert, lgamma
; inline.ks:
;  * $inline
; mnistcnn.ks:
;  * Vector of Vectors
;  * sumbuild, ts_add
; power.ks:
;  * Auto-derivation (rev$, fwd$)
; test[0-4].ks
;  * Strings
;  * $check
; vprod.ks:
;  * (lam) as function argument

; Assorted tests

; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/awf.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/bool.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/comment.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/CA-subst.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/hello-world.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/lmzero.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/logsumexp.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/negative-float-literals.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/sum.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/test_stopgrad.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/tm-rev.ks 2> /dev/null

; Examples

; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex0.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex1.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex2.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex3.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex4.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex5.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex6.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex7.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/ex8.ks 2> /dev/null

; Mul

; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/mul4.ks 2> /dev/null
; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/mul8.ks 2> /dev/null
