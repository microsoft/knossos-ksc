; Some examples so we can inspect whether simple linear algebra code
; causes our our codegen to produce deltaVecs that are tricky to get
; rid of.

(def mul_mat_vec
     (Vec Float)
     ((w : Vec (Vec Float))
      (image : Vec Float))
   (let ((o (size w))
         (k (size (index 0 w))))
     (build o (lam (oi : Integer)
     (sumbuild k (lam (ki : Integer)
       (mul (index ki (index oi w))
          (index ki image))
       ))))))

(gdef fwd [mul_mat_vec (Tuple (Vec (Vec Float)) (Vec Float))])
(gdef rev [mul_mat_vec (Tuple (Vec (Vec Float)) (Vec Float))])
(gdef suffwdpass [mul_mat_vec (Tuple (Vec (Vec Float)) (Vec Float))])
(gdef sufrevpass [mul_mat_vec (Tuple (Vec (Vec Float)) (Vec Float))])
(gdef sufrev [mul_mat_vec (Tuple (Vec (Vec Float)) (Vec Float))])

(def mul_mat_mat
     (Vec (Vec Float))
     ((x : Vec (Vec Float))
      (y : Vec (Vec Float)))
   (let ((rx (size x))
         (rycx (size y))
         (cy (size (index 0 y))))
     (build rx (lam (rxi   : Integer)
     (build cy (lam (cyi : Integer)
     (sumbuild rycx (lam (rycx : Integer)
         (mul (index rycx (index rxi  x))
              (index cy   (index rycx y)))
       ))))))))

(gdef fwd [mul_mat_mat (Tuple (Vec (Vec Float)) (Vec (Vec Float)))])
(gdef rev [mul_mat_mat (Tuple (Vec (Vec Float)) (Vec (Vec Float)))])
(gdef suffwdpass [mul_mat_mat (Tuple (Vec (Vec Float)) (Vec (Vec Float)))])
(gdef sufrevpass [mul_mat_mat (Tuple (Vec (Vec Float)) (Vec (Vec Float)))])
(gdef sufrev [mul_mat_mat (Tuple (Vec (Vec Float)) (Vec (Vec Float)))])

(def mul_vec_mat
     (Vec Float)
     ((w : Vec (Vec Float))
      (image : Vec Float))
     (let ((o (size (index 0 w)))   ; Ugh!
           (k (size w)))
     (build o (lam (oi : Integer)
     (sumbuild k (lam (ki : Integer)
       (mul (index oi (index ki w))
          (index ki image))
       ))))))

(gdef fwd [mul_vec_mat (Tuple (Vec (Vec Float)) (Vec Float))])
(gdef rev [mul_vec_mat (Tuple (Vec (Vec Float)) (Vec Float))])
(gdef suffwdpass [mul_vec_mat (Tuple (Vec (Vec Float)) (Vec Float))])
(gdef sufrevpass [mul_vec_mat (Tuple (Vec (Vec Float)) (Vec Float))])
(gdef sufrev [mul_vec_mat (Tuple (Vec (Vec Float)) (Vec Float))])

(def max_ Float ((x1 : Float) (x2 : Float)) (if (gt x1 x2) x1 x2))

(gdef fwd [max_ (Tuple Float Float)])
(gdef rev [max_ (Tuple Float Float)])
(gdef suffwdpass [max_ (Tuple Float Float)])
(gdef sufrevpass [max_ (Tuple Float Float)])
(gdef sufrev [max_ (Tuple Float Float)])

(def maxpool
     (Vec Float)
     (image : Vec Float)
     (build (div (size image) 2) (lam (ni : Integer)
       (max_ (index (mul 2 ni) image)
             (index (add 1 (mul 2 ni)) image)))))

(gdef fwd [maxpool (Vec Float)])
(gdef rev [maxpool (Vec Float)])
(gdef suffwdpass [maxpool (Vec Float)])
(gdef sufrevpass [maxpool (Vec Float)])
(gdef sufrev [maxpool (Vec Float)])

; The two input vectors have different lengths which makes it hard to
; find an optimisation rule to optimise the reverse mode "linear map
; AD" code.
(def maxpoolVec
     (Vec Float)
     ((image : (Vec Float))
      (image2 : (Vec Float)))
     (build (div (size image) 2) (lam (ni : Integer)
         (max_ (max_ (index (mul 2 ni) image)
                     (index (add 1 (mul 2 ni)) image))
               (index ni image2)))))


(gdef fwd [maxpoolVec (Tuple (Vec Float) (Vec Float))])
(gdef rev [maxpoolVec (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [maxpoolVec (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [maxpoolVec (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [maxpoolVec (Tuple (Vec Float) (Vec Float))])

(def maxpoolVec_ANF
     (Vec Float)
     ((image : (Vec Float))
      (image2 : (Vec Float)))
     (build (div (size image) 2) (lam (ni : Integer)
         (let ((ni2 (mul 2 ni))
               (ni21 (add ni2 1))
               (image_ni2 (index ni2 image))
               (image_ni21 (index ni21 image))
               (image2_ni (index ni image2))
               (m1 (max_ image_ni2 image_ni21))
               (m2 (max_ m1 image2_ni)))
           m2))))

(gdef fwd [maxpoolVec_ANF (Tuple (Vec Float) (Vec Float))])
(gdef rev [maxpoolVec_ANF (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [maxpoolVec_ANF (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [maxpoolVec_ANF (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [maxpoolVec_ANF (Tuple (Vec Float) (Vec Float))])

; This function stands in for one that could actually be expensive
(def expensive Float ((x1 : Float) (x2 : Float)) 0.0)

(gdef fwd [expensive (Tuple Float Float)])
(gdef rev [expensive (Tuple Float Float)])
(gdef suffwdpass [expensive (Tuple Float Float)])
(gdef sufrevpass [expensive (Tuple Float Float)])
(gdef sufrev [expensive (Tuple Float Float)])

; An example to show that if we pool with an expensive function it's
; harder to eliminate the deltaVecs
(def expensivepool
     (Vec Float)
     (image : Vec Float)
     (build (div (size image) 2) (lam (ni : Integer)
       (expensive (index (mul 2 ni) image)
                  (index (add 1 (mul 2 ni)) image)))))

(gdef fwd [expensivepool (Vec Float)])
(gdef rev [expensivepool (Vec Float)])
(gdef suffwdpass [expensivepool (Vec Float)])
(gdef sufrevpass [expensivepool (Vec Float)])
(gdef sufrev [expensivepool (Vec Float)])

(def conv1d
     (Vec (Vec Float))
     ((kernels : Vec (Vec (Vec Float)))
      (image : Vec (Vec Float)))
   (let ((k (size kernels))
         (kernels_elt (index 0 kernels))
         (l (size kernels_elt))
         (kn (size (index 0 kernels_elt)))
         (l2 (size image))
         (n  (size (index 0 image))))
     (assert (eq l l2)
     (build k (lam (ki : Integer)
     (build n (lam (ni : Integer)
     (sumbuild kn (lam (kni : Integer)
     (sumbuild l  (lam (li  : Integer)
       (let ((knc (div kn 2))
             (noi (sub (add ni knc) kni))
             (outside_image (or (lt noi 0) (gte noi n)))
             (image_noi
              (if outside_image 0.0 (index noi (index li image)))))
         (mul image_noi (index kni (index li (index ki kernels))))
         ))))))))))))

(gdef fwd [conv1d (Tuple (Vec (Vec (Vec Float))) (Vec (Vec Float)))])
(gdef rev [conv1d (Tuple (Vec (Vec (Vec Float))) (Vec (Vec Float)))])
(gdef suffwdpass [conv1d (Tuple (Vec (Vec (Vec Float))) (Vec (Vec Float)))])
(gdef sufrevpass [conv1d (Tuple (Vec (Vec (Vec Float))) (Vec (Vec Float)))])
(gdef sufrev [conv1d (Tuple (Vec (Vec (Vec Float))) (Vec (Vec Float)))])

(def main Integer () 0)
