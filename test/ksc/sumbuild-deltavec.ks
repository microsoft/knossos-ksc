; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f Float ((a : (Tuple Float Float)) (b : Float))
    b)

(def g (Vec Float) ((a : Vec Float) (b : Vec Float) (c : Float))
    (sumbuild (size a) (lam (i : Integer)
        (let ((ea (index i a))
              (eb (index i b)))
          (deltaVec (size a) i (f (tuple ea eb) c))))))
