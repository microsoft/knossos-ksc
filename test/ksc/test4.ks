; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def test_tuple Integer ((x : Tuple (Vec Float) (Vec (Vec Float)) Integer))
    (add 1 (if (lt 2 3) 4 5)))


(gdef fwd [test_tuple (Tuple (Vec Float) (Vec (Vec Float)) Integer)])
(gdef rev [test_tuple (Tuple (Vec Float) (Vec (Vec Float)) Integer)])

(def g Float ((w : Tuple Float Integer))
    (get$1$2 w))

(gdef fwd [g (Tuple Float Integer)])
(gdef rev [g (Tuple Float Integer)])

(def g2 Float ((wishart : Tuple Float Integer))
    (let ((wishart_gamma (get$1$2 wishart))
         (f (mul 2.2 wishart_gamma))
         )
      f))

(gdef fwd [g2 (Tuple Float Integer)])
(gdef rev [g2 (Tuple Float Integer)])

(def main Integer ()
    (let (w (tuple 3.3 4))
        (print (g w) "\n"
            ([rev g] w 1.0)
            )))
