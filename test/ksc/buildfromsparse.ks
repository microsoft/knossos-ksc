
(def tensor2_from_vecvec (Tensor 2 Float) (vv : (Tensor 1 (Tensor 1 Float)))
    (build (tuple (size vv) (size (index 0 vv)))
           (lam (ij : (Tuple Integer Integer))
               (let ((i j) ij)
                    (index j (index i vv))))))

(gdef fwd [tensor2_from_vecvec (Tensor 1 (Tensor 1 Float))])
(gdef rev [tensor2_from_vecvec (Tensor 1 (Tensor 1 Float))])

(def tensor2_from_vecvec (Tensor 2 (Tuple Integer Float)) (vv : (Tensor 1 (Tensor 1 (Tuple Integer Float))))
    (build (tuple (size vv) (size (index 0 vv)))
           (lam (ij : (Tuple Integer Integer))
               (let ((i j) ij)
                    (index j (index i vv))))))

(gdef fwd [tensor2_from_vecvec (Tensor 1 (Tensor 1 (Tuple Integer Float)))])
(gdef rev [tensor2_from_vecvec (Tensor 1 (Tensor 1 (Tuple Integer Float)))])

(def tensor2_from_vecvec (Tensor 2 (Tuple (Tuple Integer Float) (Tuple (Tuple Integer Integer) Float)))
         (vv : (Tensor 1 (Tensor 1 (Tuple (Tuple Integer Float) (Tuple (Tuple Integer Integer) Float)))))
    (build (tuple (size vv) (size (index 0 vv)))
           (lam (ij : (Tuple Integer Integer))
               (let ((i j) ij)
                    (index j (index i vv))))))

(gdef fwd [tensor2_from_vecvec
      (Tensor 1 (Tensor 1
       (Tuple (Tuple Integer Float) (Tuple (Tuple Integer Integer) Float))))])
(gdef rev [tensor2_from_vecvec
      (Tensor 1 (Tensor 1
       (Tuple (Tuple Integer Float) (Tuple (Tuple Integer Integer) Float))))])

(def main Integer ()
    (print
        "TESTS FOLLOW"
        "\n----\n"
        "buildFromSparse builds a vector\n"
        (let (indexvals (Vec_init (tuple 1 1.0) (tuple 3 3.0) (tuple 0 5.0) (tuple 1 1.5)))
             (eq (buildFromSparse (constVec 5 (tuple))
                                  (size indexvals)
                                  (lam (i : Integer) (index i indexvals)))
                 (Vec_init 5.0 2.5 0.0 3.0 0.0)))
        
        "\n----\n"
        "buildFromSparse builds a vector from a 2D list of pairs\n"
        (let (indexvals (tensor2_from_vecvec (Vec_init (Vec_init (tuple 1 1.0) (tuple 3 3.0) (tuple 0 5.0))
                                                       (Vec_init (tuple 1 1.5) (tuple 1 9.0) (tuple 0 -1.0)))))
             (eq (buildFromSparse (constVec 5 (tuple))
                                  (size indexvals)
                                  (lam (ij : (Tuple Integer Integer)) (index ij indexvals)))
                 (Vec_init 4.0 11.5 0.0 3.0 0.0)))
        
        "\n----\n"
        "buildFromSparse builds a tensor\n"
        (let (indexvals (Vec_init (tuple (tuple 1 2) 1.0)
                                  (tuple (tuple 1 3) 4.0)
                                  (tuple (tuple 0 2) 5.0)
                                  (tuple (tuple 0 0) 8.0)
                                  (tuple (tuple 1 2) 2.0)))
             (eq (buildFromSparse (constVec (tuple 2 4) (tuple))
                                  (size indexvals)
                                  (lam (i : Integer) (index i indexvals)))
                 (tensor2_from_vecvec (Vec_init (Vec_init 8.0 0.0 5.0 0.0)
                                                (Vec_init 0.0 0.0 3.0 4.0)))))
        
        "\n----\n"
        "buildFromSparse builds a jagged vec of vec\n"
        (let (indexvals (Vec_init (tuple 1 (Vec_init 1.0 3.0 5.0))
                                  (tuple 0 (Vec_init 5.0 3.0 6.0 9.0))
                                  (tuple 4 (Vec_init 0.0 4.0))
                                  (tuple 1 (Vec_init 1.0 6.0 2.0))))
             (eq (buildFromSparse (Vec_init (constVec 4 (tuple))
                                            (constVec 3 (tuple))
                                            (constVec 1 (tuple))
                                            (constVec 4 (tuple))
                                            (constVec 2 (tuple)))
                                  (size indexvals)
                                  (lam (i : Integer) (index i indexvals)))
                 (Vec_init (Vec_init 5.0 3.0 6.0 9.0)
                           (Vec_init 2.0 9.0 7.0)
                           (constVec 1 0.0)
                           (constVec 4 0.0)
                           (Vec_init 0.0 4.0))))

        "\n----\n"
        "buildFromSparseTupled builds two tensors\n"
        (let (indexvals (Vec_init (tuple (tuple 0 3.0) (tuple (tuple 0 1) 5.0))
                                  (tuple (tuple 2 4.0) (tuple (tuple 1 3) 9.0))
                                  (tuple (tuple 2 1.0) (tuple (tuple 0 2) 8.0))
                                  (tuple (tuple 4 3.0) (tuple (tuple 0 1) 1.0))))
             (eq (buildFromSparseTupled (tuple (constVec 5 (tuple)) (constVec (tuple 2 4) (tuple)))
                                        (size indexvals)
                                        (lam (i : Integer) (index i indexvals)))
                 (tuple (Vec_init 3.0 0.0 5.0 0.0 3.0)
                        (tensor2_from_vecvec (Vec_init (Vec_init 0.0 6.0 8.0 0.0)
                                                       (Vec_init 0.0 0.0 0.0 9.0))))))
        
        "\n----\n"
        "buildFromSparseTupled builds two tensors from a 2D list of values\n"
        (let (indexvals (tensor2_from_vecvec (Vec_init (Vec_init (tuple (tuple 0 1.0) (tuple (tuple 0 1) 5.0))
                                                                 (tuple (tuple 2 4.0) (tuple (tuple 1 3) 9.0))
                                                                 (tuple (tuple 2 1.0) (tuple (tuple 0 2) 8.0)))
                                                       (Vec_init (tuple (tuple 1 2.0) (tuple (tuple 0 1) 1.0))
                                                                 (tuple (tuple 2 3.0) (tuple (tuple 0 0) 4.0))
                                                                 (tuple (tuple 4 6.0) (tuple (tuple 0 0) 3.0))))))
             (eq (buildFromSparseTupled (tuple (constVec 5 (tuple)) (constVec (tuple 2 4) (tuple)))
                                        (size indexvals)
                                        (lam (ij : (Tuple Integer Integer)) (index ij indexvals)))
                 (tuple (Vec_init 1.0 2.0 8.0 0.0 6.0)
                        (tensor2_from_vecvec (Vec_init (Vec_init 7.0 6.0 8.0 0.0)
                                                       (Vec_init 0.0 0.0 0.0 9.0))))))
        "\n"))
