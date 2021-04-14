; RUN: ksc-mlir MLIR %s  | FileCheck %s --check-prefix=MLIR

(edef [f Float] Float (Float))
; MLIR: func private @f$af(f64) -> f64

(edef [g (Tuple Float Integer)] Float (Float Integer))
; MLIR: func private @g$afi(f64, i64) -> f64

(edef [rev [f Float]] Float (Float Float))
; MLIR: func private @rev$f$af(f64, f64) -> f64

(def [h Float] Float (a : Float)
    ([rev [f Float]] ([f Float] a) a))
; MLIR: func private @h$af(%arg0: f64) -> f64 {
; MLIR-NEXT: %[[f:[0-9]+]] = call @f$af(%arg0) : (f64) -> f64
; MLIR-NEXT: %[[revf:[0-9]+]] = call @rev$f$af(%[[f]], %arg0) : (f64, f64) -> f64
; MLIR-NEXT: return %[[revf]] : f64

