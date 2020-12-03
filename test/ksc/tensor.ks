

(def idMat (Tensor 2 Float) (M : (Tensor 2 Float))
    M)

(def testElement Float ((i : Integer) (j : Integer) (k : Integer))
    (add (add (mul 2.0 (to_float i))
              (mul 10.0 (to_float j)))
         (mul 100.0 (to_float k))))

(def main Integer ()
    (let (vvv (build 2 (lam (i : Integer)
                  (build 4 (lam (j : Integer)
                      (build 5 (lam (k : Integer)
                          (testElement i j k))))))))
    (let (t (build (tuple 2 4 5) (lam (index : (Tuple Integer Integer Integer))
                (let ((i j k) index)
                    (testElement i j k)))))
      (print
          "\n----\n" 
          "TESTS FOLLOW"

          "\n----\n"
          "Tensor size\n"
          (eq (size t)
              (tuple (size vvv) (size (index 0 vvv)) (size (index 0 (index 0 vvv)))))

          "\n----\n"
          "Tensor index\n"
          (eq (index (tuple 1 2 3) t)
              (index 3 (index 2 (index 1 vvv))))
      ))))

