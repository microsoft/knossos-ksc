; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float ((x : Float) (y : Float))
     (let (z (mul@ff x y))
       (add@ff (mul@ff z x) (mul@ff z y))))

(def rev_f (Tuple Float Float)
     ((x : Float) (y : Float) (d_r : Float))
     (let (z (mul@ff x y))
          (tuple (add@ff (add@ff (mul@ff y (mul@ff x d_r))
                       (mul@ff z d_r))
                    (mul@ff y (mul@ff y d_r)))
                 (add@ff (mul@ff x (mul@ff x d_r))
                    (add@ff (mul@ff x (mul@ff y d_r))
                       (mul@ff z d_r))))))

(def fst_rev_f Float
     ((x : Float) (y : Float) (d_r : Float))
     (let (z (mul@ff x y))
       (get$1$2
          (tuple (add@ff (add@ff (mul@ff y (mul@ff x d_r))
                       (mul@ff z d_r))
                    (mul@ff y (mul@ff y d_r)))
                 (add@ff (mul@ff x (mul@ff x d_r))
                    (add@ff (mul@ff x (mul@ff y d_r))
                       (mul@ff z d_r)))))))

(def fst_rev_f_inline Float
     ((x : Float) (y : Float) (d_r : Float))
     (get$1$2 (rev_f x y d_r)))

(def fst_rev_f_inline_prim Float
     ((x : Float) (y : Float) (d_r : Float))
     (get$1$2 ($inline (rev_f x y d_r))))
