; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(edef edef_example Float (Float))
(edef [D edef_example] (LM Float Float) (Float))
(edef [Dt edef_example] (Tuple Float (LM Float Float)) (Float))
(edef R$edef_example (LM Float Float) (Float))
(edef [fwd edef_example] Float (Float Float))
(edef [fwdt edef_example] Float (Float Float))
(edef [rev edef_example] Float (Float Float))

(def g Float ((x : Float) (y : Float)) (add (edef_example x) y))

(def main Integer () 0)
