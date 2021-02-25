; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def constVec_ (Vec Float) ((n : Integer) (x : Float))
     (build n (lam (i : Integer) x)))

(gdef fwd [constVec_ (Tuple Integer Float)])
(gdef rev [constVec_ (Tuple Integer Float)])
(gdef suffwdpass [constVec_ (Tuple Integer Float)])
(gdef sufrevpass [constVec_ (Tuple Integer Float)])
(gdef sufrev [constVec_ (Tuple Integer Float)])

(def indexAgain Float ((i : Integer) (x : Vec Float))
     (index i x))

(gdef fwd [indexAgain (Tuple Integer (Vec Float))])
(gdef rev [indexAgain (Tuple Integer (Vec Float))])
(gdef suffwdpass [indexAgain (Tuple Integer (Vec Float))])
(gdef sufrevpass [indexAgain (Tuple Integer (Vec Float))])
(gdef sufrev [indexAgain (Tuple Integer (Vec Float))])

(def simplest (Vec Float) (x : Vec Float)
     (build (size x) (lam (i : Integer) (index i x))))

(gdef fwd [simplest (Vec Float)])
(gdef rev [simplest (Vec Float)])
(gdef suffwdpass [simplest (Vec Float)])
(gdef sufrevpass [simplest (Vec Float)])
(gdef sufrev [simplest (Vec Float)])

(def mulvec (Vec Float) ( (x : Vec Float) (y : Vec Float) )
     (build (size x) (lam (i : Integer) (mul (index i x) (index i y)))))

(gdef fwd [mulvec (Tuple (Vec Float) (Vec Float))])
(gdef rev [mulvec (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [mulvec (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [mulvec (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [mulvec (Tuple (Vec Float) (Vec Float))])

(def f6 Float ( (x : Vec Float) (y : Vec Float) )
        (sum (mulvec x y)))

(gdef fwd [f6 (Tuple (Vec Float) (Vec Float))])
(gdef rev [f6 (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [f6 (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [f6 (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [f6 (Tuple (Vec Float) (Vec Float))])

(def mkfloat Float ((seed  : Integer)
                    (scale : Float))
       (mul ($ranhashdoub seed) scale))

(def mkvec (Vec Float) ((seed  : Integer)
                        (n     : Integer)
                        (scale : Float))
    (build n (lam (j : Integer) (mkfloat (add j seed) scale))))

(def main Integer ()
     (let ((scale_unity 1.0)
           (N 5)
           (seed 0)
           (x (mkvec (add seed 0)    N scale_unity))
           (y (mkvec (add seed 1000) N scale_unity)))
       (print "constVec_\n"
              ([rev constVec_] (tuple N 0.0) x)
              "\n"
              ([sufrev constVec_] (tuple N 0.0) x)
              "\n"
              "indexAgain\n"
              ([rev indexAgain] (tuple 0 x) 1.0)
              "\n"
              ([sufrev indexAgain] (tuple 0 x) 1.0)
              "\n"
              "simplest\n"
              ([rev simplest] x y)
              "\n"
              ([sufrev simplest] x y)
              "\n"
              "f6\n"
              ([rev f6] (tuple x y) 1.0)
              "\n"
              ([sufrev f6] (tuple x y) 1.0)
              "\n")))
