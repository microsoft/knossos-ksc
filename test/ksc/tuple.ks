(def f Float ((t : Tuple Float Float) (n : Integer))
     (let ((x y) t)
       1.0))

(gdef fwd [f (Tuple (Tuple Float Float) Integer)])
(gdef rev [f (Tuple (Tuple Float Float) Integer)])
(gdef suffwdpass [f (Tuple (Tuple Float Float) Integer)])
(gdef sufrevpass [f (Tuple (Tuple Float Float) Integer)])
(gdef sufrev [f (Tuple (Tuple Float Float) Integer)])

(def foofilter_comp Float ((_xs$o1 : (Tensor 1 Float)))
     (let (_36 false)
     (let (_34 (tuple))
     1.2)))

(gdef fwd [foofilter_comp (Tensor 1 Float)])
(gdef rev [foofilter_comp (Tensor 1 Float)])

(def f_no_args Float () 3.14159)

(gdef fwd [f_no_args (Tuple)])
(gdef rev [f_no_args (Tuple)])

(def circumference Float (r : Float) (mul (mul 2.0 (f_no_args)) r))
(def circumference2 Float (r : Float) (mul (mul 2.0 (f_no_args (tuple))) r))

(gdef fwd [circumference Float])
(gdef rev [circumference Float])
(gdef fwd [circumference2 Float])
(gdef rev [circumference2 Float])

(def main Integer () 0)
