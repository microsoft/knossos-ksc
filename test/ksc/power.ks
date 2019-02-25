(def f Float ( (n : Integer) (x : Float) )
       (if (== n 1) x (* x (f (- n 1) x))))
