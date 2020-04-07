; RUN: ksc-mlir LLVM %s 2> /dev/null

; This is a simple program that exposes some concepts of the IR and can be used
; on a rewrite engine, with some hints of what we want to see.

; (x - 10) + (y + 20) -> (x + y) + (20 - 10) -> (x + y) + 10
(def long_expr Integer ((x : Integer) (y : Integer))
  (add (sub x 10) (add y 20)))

; (x - 10) + (y - (x - 10)) + 20 -> let (z = x - 10): z + y - z + 20 -> (y + 20)
(def repeated_expr Integer ((x : Integer) (y : Integer))
  (add (add (sub x 10) (sub y (sub x 10))) 20))

; z is computed, but never used
(def useless_expr Integer ((x : Integer) (y : Integer))
  (let (z (add x y))
       (sub x y)))

(def main Integer ()
  (pr "Constant folding"
      (long_expr 42 38)
      "Common sub-expression elimination"
      (repeated_expr 17 31)
      "Dead code elimination"
      (useless_expr 21 137)))
