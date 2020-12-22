

(def idMat (Tensor 2 Float) (M : (Tensor 2 Float))
    M)

(def testElement Float ((i : Integer) (j : Integer) (k : Integer) (c : Float))
    (add (add (add (mul 2.0 (to_float i))
                   (mul 10.0 (to_float j)))
              (mul 100.0 (to_float k)))
         c))

(def main Integer ()
    (let (vvv (build 2 (lam (i : Integer)
                  (build 4 (lam (j : Integer)
                      (build 5 (lam (k : Integer)
                          (testElement i j k 1.0))))))))
    (let (t (build (tuple 2 4 5) (lam (ijk : (Tuple Integer Integer Integer))
                (let ((i j k) ijk)
                    (testElement i j k 1.0)))))
    (let (vvv2 (build 2 (lam (i : Integer)
                  (build 4 (lam (j : Integer)
                      (build 5 (lam (k : Integer)
                          (testElement i j k 2.5))))))))
    (let (t2 (build (tuple 2 4 5) (lam (ijk : (Tuple Integer Integer Integer))
                (let ((i j k) ijk)
                    (testElement i j k 2.5)))))
      (print
          "t = " t

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

          "\n----\n"
          "Tensor equality\n"
          (eq t
              (build (size t) (lam (ijk : (Tuple Integer Integer Integer))
                  (let ((i j k) ijk)
                      (index k (index j (index i vvv)))))))

          "\n----\n"
          "Tensor inequality\n"
          (ne t
              (build (size t) (lam (ijk : (Tuple Integer Integer Integer))
                  (let ((i j k) ijk)
                      (if (eq ijk (tuple 1 3 3))
                          1.25
                          (index k (index j (index i vvv))))))))

          "\n----\n"
          "Tensor addition\n"
          (let (vvv_sum (ts_add vvv vvv2))
          (let (t_sum (ts_add t t2))
          (eq (index (tuple 1 3 2) t_sum)
              (index 2 (index 3 (index 1 vvv_sum))))))

          "\n----\n"
          "Tensor sum\n"
          (eq (sum t)
              (sumbuild (size vvv) (lam (i : Integer)
                  (sumbuild (size (index 0 vvv)) (lam (j : Integer)
                      (sum (index j (index i vvv))))))))
        
          "\n----\n"
          "Tensor sumbuild\n"
          (eq (sumbuild (size t) (lam (ijk : (Tuple Integer Integer Integer))
                  (index ijk t)))
              (sum t))
        
          "\n----\n"
          "Tensor constVec\n"
          (eq (constVec (tuple 2 3) 2.0)
              (build (tuple 2 3) (lam (ij : (Tuple Integer Integer)) 2.0)))
        
          "\n----\n"
          "Tensor deltaVec\n"
          (eq (deltaVec (tuple 2 4) (tuple 1 2) 2.0)
              (build (tuple 2 4) (lam (ij : (Tuple Integer Integer))
                  (if (eq ij (tuple 1 2)) 2.0 0.0))))
        
          "\n----\n"
          "Tensor deltaVec with index 0 out of range\n"
          (eq (deltaVec (tuple 2 4) (tuple 2 1) 2.0)
              (build (tuple 2 4) (lam (ij : (Tuple Integer Integer)) 0.0)))
        
          "\n----\n"
          "Tensor deltaVec with index 1 out of range\n"
          (eq (deltaVec (tuple 2 4) (tuple 0 4) 2.0)
              (build (tuple 2 4) (lam (ij : (Tuple Integer Integer)) 0.0)))
      ))))))

