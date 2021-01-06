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

(def max_ Float ((x1 : Float) (x2 : Float)) (if (gt x1 x2) x1 x2))

(def maxpool
     (Vec Float)
     (image : Vec Float)
     (build (div (size image) 2) (lam (ni : Integer)
       (max_ (index (mul 2 ni) image)
             (index (add 1 (mul 2 ni)) image)))))

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

; This function stands in for one that could actually be expensive
(def expensive Float ((x1 : Float) (x2 : Float)) 0.0)

; An example to show that if we pool with an expensive function it's
; harder to eliminate the deltaVecs
(def expensivepool
     (Vec Float)
     (image : Vec Float)
     (build (div (size image) 2) (lam (ni : Integer)
       (expensive (index (mul 2 ni) image)
                  (index (add 1 (mul 2 ni)) image)))))

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

(def main Integer () 0)
