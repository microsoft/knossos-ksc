; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float ((x : Float) (y : Float))
     (let (z (mul x y))
       (add (mul z x) (mul z y))))

(gdef fwd [f (Tuple Float Float)])
(gdef rev [f (Tuple Float Float)])

(def rev_f (Tuple Float Float)
     ((x : Float) (y : Float) (d_r : Float))
     (let (z (mul x y))
          (tuple (add (add (mul y (mul x d_r))
                       (mul z d_r))
                    (mul y (mul y d_r)))
                 (add (mul x (mul x d_r))
                    (add (mul x (mul y d_r))
                       (mul z d_r))))))

(gdef fwd [rev_f (Tuple Float Float Float)])
(gdef rev [rev_f (Tuple Float Float Float)])

(def fst_rev_f Float
     ((x : Float) (y : Float) (d_r : Float))
     (let (z (mul x y))
       (get$1$2
          (tuple (add (add (mul y (mul x d_r))
                       (mul z d_r))
                    (mul y (mul y d_r)))
                 (add (mul x (mul x d_r))
                    (add (mul x (mul y d_r))
                       (mul z d_r)))))))

(gdef fwd [fst_rev_f (Tuple Float Float Float)])
(gdef rev [fst_rev_f (Tuple Float Float Float)])

(def fst_rev_f_inline Float
     ((x : Float) (y : Float) (d_r : Float))
     (get$1$2 (rev_f x y d_r)))

(gdef fwd [fst_rev_f_inline (Tuple Float Float Float)])
(gdef rev [fst_rev_f_inline (Tuple Float Float Float)])

(def fst_rev_f_inline_prim Float
     ((x : Float) (y : Float) (d_r : Float))
     (get$1$2 ($inline (rev_f x y d_r))))

(gdef fwd [fst_rev_f_inline_prim (Tuple Float Float Float)])
(gdef rev [fst_rev_f_inline_prim (Tuple Float Float Float)])

(def main Integer () 0)
