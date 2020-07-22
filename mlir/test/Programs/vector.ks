; RUN: ksc-mlir LLVM %s 2> /dev/null

; Some simple vector simplifications

; (size (build K (i)) -> K
(def size_build Integer ()
  (size (build 10 (lam (i : Integer) i)))) ; 10

; (size (build x (i)) -> x
(def size_build_var Integer (x : Integer)
  (size (build x (lam (i : Integer) i)))) ; x

; (index N (build K (i)) -> N
(def index_build Integer ()
  (index 5 (build 10 (lam (i : Integer) i)))) ; 5

; (index x (build y (i)) -> x
; Do we need to check y > x, or assume it is user error if not?
(def index_build_var Integer ((x : Integer) (y : Integer))
  (index x (build y (lam (i : Integer) i)))) ; x

(def main Integer ()
  (pr "Eliding build for size const"
      (size_build)
      "Eliding build for size arg"
      (size_build_var 4)
      "Eliding build for index const"
      (index_build)
      "Eliding build for index arg"
      (index_build_var 3 7)))
