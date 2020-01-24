; Some examples so we can inspect whether simple linear algebra code
; causes our our codegen to produce deltaVecs that are tricky to get
; rid of.

(def matrix_multiply
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

(def matrix_multiply_transpose
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

(def max_ Float ((x1 : Float) (x2 : Float)) (if (gt@ff x1 x2) x1 x2))

(def maxpool
     (Vec Float)
     (image : Vec Float)
     (build (div (size image) 2) (lam (ni : Integer)
       (max_ (index (mul 2 ni) image)
             (index (add 1 (mul 2 ni)) image)))))

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
