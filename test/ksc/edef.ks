; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(edef edef_example Float (Float))
(edef D$edef_example (LM Float Float) (Float))
(edef R$edef_example (LM Float Float) (Float))
(edef fwd$edef_example Float (Float Float))
(edef rev$edef_example Float (Float Float))

(def g Float ((x : Float) (y : Float)) (+ (edef_example x) y))
