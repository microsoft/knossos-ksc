; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float ((x : Float) (y : Float))
     (let (z (* x y))
       (+ (* z x) (* z y))))

(def rev_f (Tuple Float Float)
     ((x : Float) (y : Float) (d_r : Float))
     (let (z (* x y))
          (tuple (+ (+ (* y (* x d_r))
                       (* z d_r))
                    (* y (* y d_r)))
                 (+ (* x (* x d_r))
                    (+ (* x (* y d_r))
                       (* z d_r))))))

(def fst_rev_f Float
     ((x : Float) (y : Float) (d_r : Float))
     (let (z (* x y))
       (get$1$2
          (tuple (+ (+ (* y (* x d_r))
                       (* z d_r))
                    (* y (* y d_r)))
                 (+ (* x (* x d_r))
                    (+ (* x (* y d_r))
                       (* z d_r)))))))

(def fst_rev_f_inline Float
     ((x : Float) (y : Float) (d_r : Float))
     (get$1$2 (rev_f x y d_r)))

(def fst_rev_f_inline_prim Float
     ((x : Float) (y : Float) (d_r : Float))
     (get$1$2 ($inline (rev_f x y d_r))))
