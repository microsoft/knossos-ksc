; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f1 Float ((x :  Float) (y :  Float) (i : Integer))
        (mul (if (lt i 3) (add x 1.0) (mul 7.0 (to_float i))) y)
)

(gdef fwd [f1 (Tuple Float Float Integer)])
(gdef rev [f1 (Tuple Float Float Integer)])

(def f2 Float ((x : Vec Float) (y : Vec Float) (i : Integer) )
        (mul (if (lt i 3) (index i x) 7.0) (index i y))
)

(gdef fwd [f2 (Tuple (Vec Float) (Vec Float) Integer)])
(gdef rev [f2 (Tuple (Vec Float) (Vec Float) Integer)])

(def f7 Float ((x : Vec Float) (y : Vec Float) )
    (assert (eq (size x) (size y))
        (sum (build (size x)
                    (lam (i : Integer) (mul (if (lt i 3) (index i x) 7.0) (index i y))))))
)

(gdef fwd [f7 (Tuple (Vec Float) (Vec Float))])
(gdef rev [f7 (Tuple (Vec Float) (Vec Float))])

(def main Integer ()
    (let (v1 (build 3 (lam (i : Integer) (mul 3.0 (to_float i)))))
        (print 
            (f7 v1 v1) "\n"
            ; See https://github.com/awf/knossos/issues/281 (D$f7 v1 v1)
            ; See https://github.com/awf/knossos/issues/281 (D$f1 1.1 2.3 2)
            ([fwd f1] (tuple 1.1 2.3 3) (tuple 0.0 1.0 (tuple))) "\n"
            ([fwd f1] (tuple 1.1 2.3 3) (tuple 1.0 0.0 (tuple))) "\n"
            )))
