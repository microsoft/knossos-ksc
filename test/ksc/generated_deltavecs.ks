; Some examples so we can inspect whether simple linear algebra code
; causes our our codegen to produce deltaVecs that are tricky to get
; rid of.

(def matrix_multiply
     (Vec o Float)
     ((w : Vec o (Vec k Float))
      (image : Vec k Float))
     (build o (lam (oi : Integer)
     (sumbuild k (lam (ki : Integer)
       (mul (index ki (index oi w))
          (index ki image))
       )))))

(def matrix_multiply_transpose
     (Vec o Float)
     ((w : Vec k (Vec o Float))
      (image : Vec k Float))
     (build o (lam (oi : Integer)
     (sumbuild k (lam (ki : Integer)
       (mul (index oi (index ki w))
          (index ki image))
       )))))

(def max_ Float ((x1 : Float) (x2 : Float)) (if (gt x1 x2) x1 x2))

(def maxpool
     (Vec (div n 2) Float)
     (image : Vec n Float)
     (build (div n 2) (lam (ni : Integer)
       (max_ (index (mul 2 ni) image)
             (index (add 1 (mul 2 ni)) image)))))

; This function stands in for one that could actually be expensive
(def expensive Float ((x1 : Float) (x2 : Float)) 0.0)

; An example to show that if we pool with an expensive function it's
; harder to eliminate the deltaVecs
(def expensivepool
     (Vec (div n 2) Float)
     (image : Vec n Float)
     (build (div n 2) (lam (ni : Integer)
       (expensive (index (mul 2 ni) image)
                  (index (add 1 (mul 2 ni)) image)))))

(def conv1d
     (Vec k (Vec n Float))
     ((kernels : Vec k (Vec l (Vec kn Float)))
      (image : Vec l (Vec n Float)))
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
         ))))))))))
