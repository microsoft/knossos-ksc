(def conv2d
     (Vec (Vec (Vec Float)))
     ((kernels : Vec (Vec (Vec (Vec Float))))
      (bias    : Vec Float)
      (image : Vec (Vec (Vec Float))))
  (let ((k (size kernels))
        (kernels_elt (index 0 kernels))
        (l (size kernels_elt))
        (kernels_elt2 (index 0 kernels_elt))
        (kn (size kernels_elt2))
        (km (size (index 0 kernels_elt2)))
        (image_elt (index 0 image))
        (n (size image_elt))
        (m (size (index 0 image_elt))))
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
               (outside_image (or (lt@ii noi 0)
                              (or (gte noi n)
                              (or (lt@ii moi 0)
                                  (gte moi m)))))
               (image_noi_moi
                (if outside_image
                    0.0
                  (index moi (index noi (index li image))))))

           (mul image_noi_moi
              (index kmi (index kni (index li (index ki kernels)))))

           )))))))
       (index ki bias))
       ))))))))

(def max_ Float ((x : Float) (y : Float)) (if (gt@ff x y) x y))

(def relu Float (x : Float) (max_ x 0.0))

(def relu3d (Vec (Vec (Vec Float)))
     (image : Vec (Vec (Vec Float)))
   (let ((k (size image))
         (image_elt (index 0 image))
         (n (size image_elt))
         (m (size (index 0 image_elt))))
     (build k (lam (ki : Integer)
     (build n (lam (ni : Integer)
     (build m (lam (mi : Integer)
     (relu (index mi (index ni (index ki image))))))))))))

(def relu1d (Vec Float)
     (image : Vec Float)
     (build (size image) (lam (ki : Integer)
     (relu (index ki image)))))

(def maxpool
     (Vec (Vec (Vec Float)))
     ((image : Vec (Vec (Vec Float))))
   (let ((l (size image))
         (image_elt (index 0 image))
         (n (size image_elt))
         (m (size (index 0 image_elt))))
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
         )))))))))

(def dense3d
     (Vec Float)
     ((w : Vec (Vec (Vec (Vec Float))))
      (bias : Vec Float)
      (image : Vec (Vec (Vec Float))))
  (let ((o (size w))
        (k (size image))
        (image_elt (index 0 image))
        (n (size image_elt))
        (m (size (index 0 image_elt))))
     (build o (lam (oi : Integer)
     (add
     (sumbuild k (lam (ki : Integer)
     (sumbuild n (lam (ni : Integer)
     (sumbuild m (lam (mi : Integer)
       (mul (index mi (index ni (index ki (index oi w))))
             (index mi (index ni (index ki image))))
       ))))))
     (index oi bias))
     ))))

; Just the matrix-vector multiply that we all know and love
(def dense1d
     (Vec Float)
     ((w : Vec (Vec Float))
      (bias : Vec Float)
      (image : Vec Float))
   (let ((o (size w))
         (k (size image)))
     (build o (lam (oi : Integer)
     (add
     (sumbuild k (lam (ki : Integer)
       (mul (index ki (index oi w))
             (index ki image))
       ))
     (index oi bias))
     ))))

(def mnist
     (Vec Float)
     ((image : Vec (Vec (Vec Float)))
      (k1  : Vec (Vec (Vec (Vec Float))))
      (bk1 : Vec Float)
      (k2  : Vec (Vec (Vec (Vec Float))))
      (bk2 : Vec Float)
      (d1  : Vec (Vec (Vec (Vec Float))))
      (bd1 : Vec Float)
      (d2  : Vec (Vec Float))
      (bd2 : Vec Float))
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
