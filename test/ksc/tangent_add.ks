; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def my_tangent_add Integer ((x : Integer) (y : Tuple)) x)

(def f Integer ((x : Integer) (y : Tuple)) (my_tangent_add x y))
