; Knossos tests, should be parseable into MLIR and LLVM.
; However, there are a number of tests that do not pass for various reasons.
; Here are the ones that do pass. If new ones are supported, add them here.

; For the inlined tests here:
; RUN: ksc-mlir LLVM %s > /dev/null

; Unsupported tests need:

; adbench-lstm.ks
;  * Vector of Tuples of Vectors
;  * constVec
; derivative-selection.ks:
;  * $inline
; edef.ks:
;  * LM type
; ex3.ks:
;  * Unknown
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

; RUN: ksc-mlir LLVM %KNOSSOS/test/ksc/awf.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/bool.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/comment.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/CA-subst.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/hello-world.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/lmzero.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/logsumexp.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/negative-float-literals.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/sum.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/test_stopgrad.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/tm-rev.ks > /dev/null

; Examples

; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/ex0.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/ex1.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/ex2.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/ex4.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/ex5.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/ex6.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/ex7.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/ex8.ks > /dev/null

; Mul

; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/mul4.ks > /dev/null
; DISABLED: ksc-mlir LLVM %KNOSSOS/test/ksc/mul8.ks > /dev/null
