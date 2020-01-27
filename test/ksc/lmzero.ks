; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h (Vec Float) (x : Vec Float)
  (build (mul@ii 2 (size x)) (lam (i : Integer) 1.0)))

(def main Integer ()
  (pr 1.1))
