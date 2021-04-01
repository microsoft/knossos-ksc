; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(edef edef_example Float (Float))
(edef [D edef_example] (LM Float Float) (Float))
(edef [Dt edef_example] (Tuple Float (LM Float Float)) (Float))
(edef R$edef_example (LM Float Float) (Float))
(edef [fwd edef_example] Float (Tuple Float Float))
(edef [fwdt edef_example] Float (Tuple Float Float))
(edef [rev edef_example] Float (Tuple Float Float))
(edef [suffwdpass edef_example] (Tuple Float (Tuple)) (Float))
(edef [sufrevpass [edef_example Float]] (Float) (Tuple Float (Tuple)))

(def g Float ((x : Float) (y : Float)) (add (edef_example x) y))
(gdef fwd [g (Tuple Float Float)])
(gdef rev [g (Tuple Float Float)])
(gdef suffwdpass [g (Tuple Float Float)])
(gdef sufrevpass [g (Tuple Float Float)])
(gdef sufrev [g (Tuple Float Float)])

(def main Integer () 0)
