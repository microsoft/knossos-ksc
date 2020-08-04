; RUN: ( ksc-mlir AST -v %s 2>&1 || true) | FileCheck %s --check-prefix=MLIR

(let ((a 1) 
      (bb "fred\\"")
      ) 
    (print a "b"; comment
        a#|
        ||#a
        a;
a
           "c;" "d"#| #| f |# |#"e"
    ))
; MLIR: ((let ((a 1) (bb "fred\\"")) (print a "b" a a a a "c;" "d" "e")))
