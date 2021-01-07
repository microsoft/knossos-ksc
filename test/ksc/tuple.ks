(def f Float ((t : Tuple Float Float) (n : Integer))
     (let ((x y) t)
       1.0))

(gdef fwd [f (Tuple (Tuple Float Float) Integer)])
(gdef rev [f (Tuple (Tuple Float Float) Integer)])

(def main Integer () 0)
