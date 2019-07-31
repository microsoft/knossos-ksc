(def conv2d
     (Vec k (Vec n (Vec m Float)))
     ((kernels : Vec k (Vec l (Vec kn (Vec km Float))))
      (bias    : Vec k Float)
      (image : Vec l (Vec n (Vec m Float))))
     (build k (lam (ki : Integer)
     (build n (lam (ni : Integer)
     (build m (lam (mi : Integer)
       (add
       (sumbuild kn (lam (kni : Integer)
       (sumbuild km (lam (kmi : Integer)
       (sumbuild l  (lam (li  : Integer)
         (let ((knc (div kn 2))
               (kmc (div km 2))
               (noi (add (sub ni knc) kni))
               (moi (add (sub mi kmc) kmi))
               (outside_image (or (< noi 0)
                              (or (>= noi n)
                              (or (< moi 0)
                                  (>= moi m)))))
               (image_noi_moi
                (if outside_image
                    0.0
                  (index moi (index noi (index li image))))))

           (mul image_noi_moi
              (index kmi (index kni (index li (index ki kernels)))))

           )))))))
       (index ki bias))
       )))))))

(def max_ Float ((x : Float) (y : Float)) (if (> x y) x y))

(def relu Float (x : Float) (max_ x 0.0))

(def relu3d (Vec k (Vec n (Vec m Float)))
     (image : Vec k (Vec n (Vec m Float)))
     (build k (lam (ki : Integer)
     (build n (lam (ni : Integer)
     (build m (lam (mi : Integer)
     (relu (index mi (index ni (index ki image)))))))))))

(def relu1d (Vec k Float)
     (image : Vec k Float)
     (build k (lam (ki : Integer)
     (relu (index ki image)))))

(def maxpool
     (Vec l (Vec (div n 2) (Vec (div m 2) Float)))
     ((image : Vec l (Vec n (Vec m Float))))
     (build l (lam (li : Integer)
     (build (div n 2) (lam (ni : Integer)
     (build (div m 2) (lam (mi : Integer)
       (let ((nid (mul 2 ni))
             (mid (mul 2 mi))
             (a00 (index mid (index nid (index li image))))
             (a01 (index (add 1 mid) (index nid (index li image))))
             (a10 (index mid (index (add 1 nid) (index li image))))
             (a11 (index (add 1 mid) (index (add 1 nid) (index li image)))))
         (max_ (max_ a00 a01) (max_ a10 a11))
         ))))))))

(def dense3d
     (Vec o Float)
     ((w : Vec o (Vec k (Vec n (Vec m Float))))
      (bias : Vec o Float)
      (image : Vec k (Vec n (Vec m Float))))
     (build o (lam (oi : Integer)
     (add
     (sumbuild k (lam (ki : Integer)
     (sumbuild n (lam (ni : Integer)
     (sumbuild m (lam (mi : Integer)
       (mul (index mi (index ni (index ki (index oi w))))
             (index mi (index ni (index ki image))))
       ))))))
     (index oi bias))
     )))

; Just the matrix-vector multiply that we all know and love
(def dense1d
     (Vec o Float)
     ((w : Vec o (Vec k Float))
      (bias : Vec o Float)
      (image : Vec k Float))
     (build o (lam (oi : Integer)
     (add
     (sumbuild k (lam (ki : Integer)
       (mul (index ki (index oi w))
             (index ki image))
       ))
     (index oi bias))
     )))

(def mnist
     (Vec 10 Float)
     ((image : Vec 1 (Vec 28 (Vec 28 Float)))
      (k1  : Vec 32 (Vec 1 (Vec 5 (Vec 5 Float))))
      (bk1 : Vec 32 Float)
      (k2  : Vec 64 (Vec 32 (Vec 5 (Vec 5 Float))))
      (bk2 : Vec 64 Float)
      (d1  : Vec 1024 (Vec 64 (Vec 7 (Vec 7 Float))))
      (bd1 : Vec 1024 Float)
      (d2  : Vec 10 (Vec 1024 Float))
      (bd2 : Vec 10 Float))
     (dense1d d2 bd2
     (relu1d
     (dense3d d1 bd1
     (maxpool
     (relu3d
     (conv2d k2 bk2
     (maxpool
     (relu3d
     (conv2d k1 bk1 image
             ))))))))))
