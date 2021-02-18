(def foo Float ((x : Float) (y : Float))
    (let (a (add (mul 2.0 x) (mul 3.0 y)))
		(let (b (add (mul 4.0 a) (mul 5.0 a)))
		  (add (mul 6.0 b) (mul 7.0 b)))))

(gdef fwd [foo (Tuple Float Float)])
(gdef rev [foo (Tuple Float Float)])
(gdef suffwdpass [foo (Tuple Float Float)])
(gdef sufrevpass [foo (Tuple Float Float)])
(gdef sufrev [foo (Tuple Float Float)])

(edef indexL (Tuple (Vec Float) Float) ((Tuple Integer (Vec Float))))

(edef [suffwdpass indexL] (Tuple (Tuple (Vec Float) Float) Integer)
                        ((Tuple Integer (Vec Float))))

(edef [sufrevpass [indexL (Tuple Integer (Vec Float))]]
      (Tuple (Tuple) (Vec Float))
      ((Tuple (Tuple (Vec Float) Float) Integer)))

(edef setAt (Tensor 1 Float) (Integer (Tensor 1 Float) Float))

(edef [suffwdpass setAt] (Tuple (Tensor 1 Float) Integer)
      (Integer (Tensor 1 Float) Float))

(edef [sufrevpass [setAt (Tuple Integer (Tensor 1 Float) Float)]]
      (Tuple (Tuple) (Tensor 1 Float) Float)
      ((Tensor 1 Float) Integer))

(def cumsum (Tensor 1 Float) (v : Tensor 1 Float)
     (let (size_v (size v))
     (let (into (constVec size_v 0.0))
     (let ((acc3 into3 v3) (ifold
       (size v)
       (lam (acc_into_v_i : Tuple (Tuple Float (Tensor 1 Float) (Tensor 1 Float)) Integer)
            (let ((acc_into_v i) acc_into_v_i)
            (let ((acc1 into1 v1) acc_into_v)
            (let ((v2 v_i) (indexL i v1))
            (let (acc2 (add acc1 v_i))
            (let (into2 (setAt i into1 acc2))
            (tuple acc2 into2 v2)))))))
       (tuple 0.0 into v)))
     into3))))

(gdef suffwdpass [cumsum (Tensor 1 Float)])
(gdef sufrevpass [cumsum (Tensor 1 Float)])
(gdef sufrev [cumsum (Tensor 1 Float)])

(def main Integer () 0)
