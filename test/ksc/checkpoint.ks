;; The examples in this file are taken from:
;;
;; Jeffrey Mark Siskind & Barak A. Pearlmutter (2018)
;; Divide-and-conquer checkpointing for arbitrary programs with no
;; user annotation, Optimization Methods and Software,33:4-6,
;; 1288-1330, DOI: 10.1080/10556788.2018.1459621
;;
;; https://engineering.purdue.edu/~qobi/papers/oms2018.pdf
;;
;; The following is another introduction to checkpointing:
;;
;; Benjamin Dauvergne and Laurent Hascoet
;; The Data-Flow Equations of Checkpointing inreverse Automatic
;; Differentiation
;;
;; https://www-sop.inria.fr/tropics/papers/DauvergneHascoet06.pdf

(def e0 Float (x : Float) (sin x))
(def e1 Float (x : Float) (sin x))
(def e2 Float (x : Float) (sin x))
(def ev Float (x : Float) (sin x))

(def has_a_big_bog Float (x : Float) (sin x))

(gdef suffwdpass [e0 Float])
(gdef sufrevpass [e0 Float])
(gdef suffwdpass [e1 Float])
(gdef sufrevpass [e1 Float])
(gdef suffwdpass [e2 Float])
(gdef sufrevpass [e2 Float])
(gdef suffwdpass [ev Float])
(gdef sufrevpass [ev Float])

(gdef suffwdpass [has_a_big_bog Float])
(gdef sufrevpass [has_a_big_bog Float])

(def without_checkpointing Float (x : Float)
     (let  (p0 (has_a_big_bog x))
     (let  (p1 (e1 p0))
     p1)))

(def with_checkpointing Float (x : Float)
     (let  (p0 (checkpoint (has_a_big_bog x)))
     (let  (p1 (e1 p0))
     p1)))

(def figure2b Float (u : Float)
     (let (p (checkpoint (e0 u)))
       (ev p)))

(def figure2c Float (u : Float)
     (let (p2 (checkpoint
       (let (p1 (checkpoint
         (let (p0 (checkpoint (e0 u)))
           (e1 p0))))
         (e2 p1))))
       (ev p2)))

(def figure2d Float (u : Float)
     (let (p0 (checkpoint (e0 u)))
     (let (p1 (checkpoint (e1 p0)))
     (let (p2 (checkpoint (e2 p1)))
       (ev p2)))))

(def figure2e Float (u : Float)
     (let (p1
       (checkpoint (let (p0 (checkpoint (e0 u)))
                     (e1 p0))))
       (let (p2 (checkpoint (e2 p1)))
         (ev p2))))

(gdef suffwdpass [without_checkpointing Float])
(gdef sufrevpass [without_checkpointing Float])
(gdef suffwdpass [with_checkpointing Float])
(gdef sufrevpass [with_checkpointing Float])
(gdef suffwdpass [figure2b Float])
(gdef sufrevpass [figure2b Float])
(gdef suffwdpass [figure2c Float])
(gdef sufrevpass [figure2c Float])
(gdef suffwdpass [figure2d Float])
(gdef sufrevpass [figure2d Float])
(gdef suffwdpass [figure2e Float])
(gdef sufrevpass [figure2e Float])

(def main Integer () 0)
