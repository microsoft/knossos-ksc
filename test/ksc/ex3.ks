; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h Float ( (x : Float) (y : Float))
       (let (z (add x y))
            (mul y z)))

(gdef fwd [h (Tuple Float Float)])
(gdef rev [h (Tuple Float Float)])
(gdef suffwdpass [h (Tuple Float Float)])
(gdef sufrevpass [h (Tuple Float Float)])
(gdef sufrev [h (Tuple Float Float)])

(def h2 Float (t : (Tuple Float Float))
     (let (((x y) t)
           (z (add x y)))
       (mul y z)))

(gdef fwd [h2 (Tuple Float Float)])
(gdef rev [h2 (Tuple Float Float)])
(gdef suffwdpass [h2 (Tuple Float Float)])
(gdef sufrevpass [h2 (Tuple Float Float)])
(gdef sufrev [h2 (Tuple Float Float)])

(def i Float ( (x : Float) (y : Float))
       (let (z (add x y))
            0.0))

(gdef fwd [i (Tuple Float Float)])
(gdef rev [i (Tuple Float Float)])
(gdef suffwdpass [i (Tuple Float Float)])
(gdef sufrevpass [i (Tuple Float Float)])
(gdef sufrev [i (Tuple Float Float)])

(def j0 Bool ((b : Bool) (b1 : Bool))
     (if b b (and b b1)))

(gdef fwd [j0 (Tuple Bool Bool)])
(gdef rev [j0 (Tuple Bool Bool)])
(gdef suffwdpass [j0 (Tuple Bool Bool)])
(gdef sufrevpass [j0 (Tuple Bool Bool)])
(gdef sufrev [j0 (Tuple Bool Bool)])

(def j Float ((x : Float) (y : Float) (t : (Tuple Float Float)))
     (if (eq (let (z (add x y))
               (mul y z))
             (let (z (add x x))
               (mul z z)))
         (let (((x y) t)
               (z (add x y)))
           (mul y z))
       (let (z (add x y))
         0.0)))

(gdef fwd [j (Tuple Float Float (Tuple Float Float))])
(gdef rev [j (Tuple Float Float (Tuple Float Float))])
(gdef suffwdpass [j (Tuple Float Float (Tuple Float Float))])
(gdef sufrevpass [j (Tuple Float Float (Tuple Float Float))])
(gdef sufrev [j (Tuple Float Float (Tuple Float Float))])

(def d2 Float ((x : Float) (y : Integer) (z : Integer) (t : Integer))
     (if (eq (tuple (add y z) (sub t y))
             (tuple t z))
         (add x (to_float y))
         0.0))

(gdef fwd [d2 (Tuple Float Integer Integer Integer)])
(gdef rev [d2 (Tuple Float Integer Integer Integer)])
(gdef suffwdpass [d2 (Tuple Float Integer Integer Integer)])
(gdef sufrevpass [d2 (Tuple Float Integer Integer Integer)])
(gdef sufrev [d2 (Tuple Float Integer Integer Integer)])

(def main Integer () 0)
