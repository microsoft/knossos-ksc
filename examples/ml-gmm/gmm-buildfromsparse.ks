; Hand-optimized GMM using buildFromSparse
; Original gmm.kso produced by ksc commit 777d0028

(edef [to_float Integer] Float (Integer))
(edef [D [to_float Integer]] (LM Integer Float) (Integer))
(def
 [fwd [to_float Integer]] Float
 ((x : Integer) (dx : (Tuple)))
 0.0)
(def
 [rev [to_float Integer]] (Tuple)
 ((x : Integer) (d_dto_float : Float))
 (tuple))
(edef
 [Dt [to_float Integer]]
 (Tuple Float (LM Integer Float))
 (Integer))
(def [not Bool] Bool (p : Bool) (if p false true))
(def [fwd [not Bool]] (Tuple) (_t1 : (Tuple Bool (Tuple))) (tuple))
(def [rev [not Bool]] (Tuple) (_t1 : (Tuple Bool (Tuple))) (tuple))
(edef [neg Float] Float (Float))
(edef [D [neg Float]] (LM Float Float) (Float))
(edef [Dt [neg Float]] (Tuple Float (LM Float Float)) (Float))
(def
 [fwd [neg Float]] Float
 ((x : Float) (dx : Float))
 ([neg Float] dx))
(def
 [rev [neg Float]] Float
 ((x : Float) (d_dneg : Float))
 ([neg Float] d_dneg))
(edef [neg Integer] Integer (Integer))
(edef [D [neg Integer]] (LM Integer Integer) (Integer))
(edef
 [Dt [neg Integer]]
 (Tuple Integer (LM Integer Integer))
 (Integer))
(def
 [fwd [neg Integer]] (Tuple)
 ((x : Integer) (dx : (Tuple)))
 (tuple))
(def
 [rev [neg Integer]] (Tuple)
 ((x : Integer) (d_dneg : (Tuple)))
 (tuple))
(edef [add (Tuple Float Float)] Float ((Tuple Float Float)))
(edef
 [D [add (Tuple Float Float)]]
 (LM (Tuple Float Float) Float)
 ((Tuple Float Float)))
(edef
 [Dt [add (Tuple Float Float)]]
 (Tuple Float (LM (Tuple Float Float) Float))
 ((Tuple Float Float)))
(def
 [fwd [add (Tuple Float Float)]] Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let (((dx1 dx2) dxt)) ([add (Tuple Float Float)] dx1 dx2)))
(def
 [rev [add (Tuple Float Float)]] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (tuple drt drt))
(edef
 [add (Tuple Integer Integer)]
 Integer
 ((Tuple Integer Integer)))
(edef
 [D [add (Tuple Integer Integer)]]
 (LM (Tuple Integer Integer) Integer)
 ((Tuple Integer Integer)))
(edef
 [Dt [add (Tuple Integer Integer)]]
 (Tuple Integer (LM (Tuple Integer Integer) Integer))
 ((Tuple Integer Integer)))
(def
 [fwd [add (Tuple Integer Integer)]] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [add (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
 (tuple (tuple) (tuple)))
(edef [sub (Tuple Float Float)] Float ((Tuple Float Float)))
(edef
 [D [sub (Tuple Float Float)]]
 (LM (Tuple Float Float) Float)
 ((Tuple Float Float)))
(edef
 [Dt [sub (Tuple Float Float)]]
 (Tuple Float (LM (Tuple Float Float) Float))
 ((Tuple Float Float)))
(def
 [fwd [sub (Tuple Float Float)]] Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let (((dx1 dx2) dxt)) ([sub (Tuple Float Float)] dx1 dx2)))
(def
 [rev [sub (Tuple Float Float)]] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (tuple drt ([neg Float] drt)))
(edef
 [sub (Tuple Integer Integer)]
 Integer
 ((Tuple Integer Integer)))
(edef
 [D [sub (Tuple Integer Integer)]]
 (LM (Tuple Integer Integer) Integer)
 ((Tuple Integer Integer)))
(edef
 [Dt [sub (Tuple Integer Integer)]]
 (Tuple Integer (LM (Tuple Integer Integer) Integer))
 ((Tuple Integer Integer)))
(def
 [fwd [sub (Tuple Integer Integer)]] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [sub (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
 (tuple (tuple) (tuple)))
(def
 [sub (Tuple (Vec Float) (Vec Float))] (Vec Float)
 ((a : Vec Float) (b : Vec Float))
 (build (size a)
        (lam (i : Integer) ([sub (Tuple Float Float)] (index i a)
                                                      (index i b)))))
(def
 [fwd [sub (Tuple (Vec Float) (Vec Float))]] (Vec Float)
 (_t1 : (Tuple (Tuple (Vec Float) (Vec Float))
               (Tuple (Vec Float) (Vec Float))))
 (let
  ((_t (get$1$2 _t1))
   (a (get$1$2 _t))
   (b (get$2$2 _t))
   (ksc$argVar (get$2$2 _t1)))
  (build (size a)
         (lam (i : Integer) ([fwd [sub (Tuple Float Float)]] (tuple (index i
                                                                           a)
                                                                    (index i b))
                                                             (tuple (index i (get$1$2 ksc$argVar))
                                                                    (index i
                                                                           (get$2$2 ksc$argVar))))))))
(def
 [rev [sub (Tuple (Vec Float) (Vec Float))]] (Tuple (Vec Float)
                                                    (Vec Float))
 (_t1 : (Tuple (Tuple (Vec Float) (Vec Float)) (Vec Float)))
 (let
  ((_t (get$1$2 _t1))
   (a (get$1$2 _t))
   (b (get$2$2 _t))
   (ksc$argVar (get$2$2 _t1))
   (t$4993 (size a)))
  (tuple (build t$4993
                (lam (i : Integer) (get$1$2 ([rev [sub (Tuple Float
                                                              Float)]] (tuple (index i a)
                                                                              (index i b))
                                                                       (index i ksc$argVar)))))
         (build t$4993
                (lam (i : Integer) (get$2$2 ([rev [sub (Tuple Float
                                                              Float)]] (tuple (index i a)
                                                                              (index i b))
                                                                       (index i ksc$argVar))))))))
(def
 [sub (Tuple (Vec Float) Float)] (Vec Float)
 ((a : Vec Float) (b : Float))
 (build (size a)
        (lam (i : Integer) ([sub (Tuple Float Float)] (index i a) b))))
(def
 [fwd [sub (Tuple (Vec Float) Float)]] (Vec Float)
 (_t1 : (Tuple (Tuple (Vec Float) Float) (Tuple (Vec Float) Float)))
 (let
  ((_t (get$1$2 _t1))
   (a (get$1$2 _t))
   (b (get$2$2 _t))
   (ksc$argVar (get$2$2 _t1)))
  (build (size a)
         (lam (i : Integer) ([fwd [sub (Tuple Float Float)]] (tuple (index i
                                                                           a)
                                                                    b)
                                                             (tuple (index i (get$1$2 ksc$argVar))
                                                                    (get$2$2 ksc$argVar)))))))
(def
 [rev [sub (Tuple (Vec Float) Float)]] (Tuple (Vec Float) Float)
 (_t1 : (Tuple (Tuple (Vec Float) Float) (Vec Float)))
 (let
  ((_t (get$1$2 _t1))
   (a (get$1$2 _t))
   (b (get$2$2 _t))
   (ksc$argVar (get$2$2 _t1))
   (t$5016 (size a)))
  (tuple (build t$5016
                (lam (i : Integer) (get$1$2 ([rev [sub (Tuple Float
                                                              Float)]] (tuple (index i a) b)
                                                                       (index i ksc$argVar)))))
         (sumbuild t$5016
                   (lam (i : Integer) (get$2$2 ([rev [sub (Tuple Float
                                                                 Float)]] (tuple (index i a) b)
                                                                          (index i
                                                                                 ksc$argVar))))))))
(edef [mul (Tuple Float Float)] Float ((Tuple Float Float)))
(edef
 [D [mul (Tuple Float Float)]]
 (LM (Tuple Float Float) Float)
 ((Tuple Float Float)))
(edef
 [Dt [mul (Tuple Float Float)]]
 (Tuple Float (LM (Tuple Float Float) Float))
 ((Tuple Float Float)))
(def
 [fwd [mul (Tuple Float Float)]] Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  (((x1 x2) xt)
   ((dx1 dx2) dxt))
  ([add (Tuple Float Float)] ([mul (Tuple Float Float)] x2 dx1)
                             ([mul (Tuple Float Float)] x1 dx2))))
(def
 [rev [mul (Tuple Float Float)]] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  (((x1 x2) xt))
  (tuple ([mul (Tuple Float Float)] drt x2)
         ([mul (Tuple Float Float)] drt x1))))
(edef
 [mul (Tuple Integer Integer)]
 Integer
 ((Tuple Integer Integer)))
(edef
 [D [mul (Tuple Integer Integer)]]
 (LM (Tuple Integer Integer) Integer)
 ((Tuple Integer Integer)))
(edef
 [Dt [mul (Tuple Integer Integer)]]
 (Tuple Integer (LM (Tuple Integer Integer) Integer))
 ((Tuple Integer Integer)))
(def
 [fwd [mul (Tuple Integer Integer)]] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [mul (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
 (tuple (tuple) (tuple)))
(def
 [mul (Tuple Float Integer)] Float
 ((a : Float) (b : Integer))
 ([mul (Tuple Float Float)] a ([to_float Integer] b)))
(def
 [mul (Tuple Float (Vec Float))] (Vec Float)
 ((r : Float) (a : Vec Float))
 (build (size a)
        (lam (i : Integer) ([mul (Tuple Float Float)] r (index i a)))))
(edef
 [mul (Tuple (Tensor 2 Float) (Vec Float))]
 (Vec Float)
 ((Tuple (Tensor 2 Float) (Vec Float))))
(def
 [shape [mul (Tuple (Tensor 2 Float) (Vec Float))]] (Vec (Tuple))
 ((m : Tensor 2 Float) (v : Vec Float))
 (constVec (get$1$2 (size m)) (tuple)))
(edef
 [D [mul (Tuple (Tensor 2 Float) (Vec Float))]]
 (LM (Tuple (Tensor 2 Float) (Vec Float)) (Vec Float))
 ((Tuple (Tensor 2 Float) (Vec Float))))
(edef
 [Dt [mul (Tuple (Tensor 2 Float) (Vec Float))]]
 (Tuple (Vec Float)
        (LM (Tuple (Tensor 2 Float) (Vec Float)) (Vec Float)))
 ((Tuple (Tensor 2 Float) (Vec Float))))
(def
 [fwd [mul (Tuple (Tensor 2 Float) (Vec Float))]] (Vec Float)
 ((M_v : (Tuple (Tensor 2 Float) (Vec Float)))
  (dM_dv : (Tuple (Tensor 2 Float) (Vec Float))))
 (let
  (((M v) M_v)
   ((dM dv) dM_dv))
  (ts_add ([mul (Tuple (Tensor 2 Float) (Vec Float))] dM v)
          ([mul (Tuple (Tensor 2 Float) (Vec Float))] M dv))))
(edef
 [rev [mul (Tuple (Tensor 2 Float) (Vec Float))]]
 (Tuple (Tensor 2 Float) (Vec Float))
 ((Tuple (Tuple (Tensor 2 Float) (Vec Float)) (Vec Float))))
(edef [div (Tuple Float Float)] Float ((Tuple Float Float)))
(edef
 [D [div (Tuple Float Float)]]
 (LM (Tuple Float Float) Float)
 ((Tuple Float Float)))
(edef
 [Dt [div (Tuple Float Float)]]
 (Tuple Float (LM (Tuple Float Float) Float))
 ((Tuple Float Float)))
(def
 [fwd [div (Tuple Float Float)]] Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  (((x1 x2) xt)
   ((dx1 dx2) dxt))
  ([div (Tuple Float Float)] ([sub (Tuple Float
                                          Float)] ([mul (Tuple Float Float)] x2 dx1)
                                                  ([mul (Tuple Float Float)] x1 dx2))
                             ([mul (Tuple Float Float)] x2 x2))))
(def
 [rev [div (Tuple Float Float)]] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  (((x1 x2) xt))
  (tuple ([div (Tuple Float Float)] drt x2)
         ([neg Float] ([div (Tuple Float Float)] ([mul (Tuple Float
                                                              Float)] x1 drt)
                                                 ([mul (Tuple Float Float)] x2 x2))))))
(edef
 [div (Tuple Integer Integer)]
 Integer
 ((Tuple Integer Integer)))
(edef
 [D [div (Tuple Integer Integer)]]
 (LM (Tuple Integer Integer) Integer)
 ((Tuple Integer Integer)))
(edef
 [Dt [div (Tuple Integer Integer)]]
 (Tuple Integer (LM (Tuple Integer Integer) Integer))
 ((Tuple Integer Integer)))
(def
 [fwd [div (Tuple Integer Integer)]] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [div (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
 (tuple (tuple) (tuple)))
(edef [pow (Tuple Float Integer)] Float ((Tuple Float Integer)))
(edef
 [D [pow (Tuple Float Integer)]]
 (LM (Tuple Float Integer) Float)
 ((Tuple Float Integer)))
(edef
 [Dt [pow (Tuple Float Integer)]]
 (Tuple Float (LM (Tuple Float Integer) Float))
 ((Tuple Float Integer)))
(def
 [fwd [pow (Tuple Float Integer)]] Float
 ((xt : (Tuple Float Integer)) (dxt : (Tuple Float (Tuple))))
 (let
  (((x n) xt)
   ((dx dn) dxt))
  ([mul (Tuple Float Float)] dx
                             ([pow (Tuple Float Integer)] x
                                                          ([sub (Tuple Integer Integer)] n 1)))))
(def
 [rev [pow (Tuple Float Integer)]] (Tuple Float (Tuple))
 ((xt : (Tuple Float Integer)) (dret : Float))
 (let
  (((x n) xt))
  (tuple ([mul (Tuple Float Float)] dret
                                    ([pow (Tuple Float Integer)] x
                                                                 ([sub (Tuple Integer Integer)] n
                                                                                                1)))
         (tuple))))
(edef [gt (Tuple Float Float)] Bool ((Tuple Float Float)))
(edef
 [D [gt (Tuple Float Float)]]
 (LM (Tuple Float Float) Bool)
 ((Tuple Float Float)))
(edef
 [Dt [gt (Tuple Float Float)]]
 (Tuple Bool (LM (Tuple Float Float) Bool))
 ((Tuple Float Float)))
(def
 [fwd [gt (Tuple Float Float)]] (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (tuple))
(def
 [rev [gt (Tuple Float Float)]] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
 (tuple 0.0 0.0))
(edef [gt (Tuple Integer Integer)] Bool ((Tuple Integer Integer)))
(edef
 [D [gt (Tuple Integer Integer)]]
 (LM (Tuple Integer Integer) Bool)
 ((Tuple Integer Integer)))
(edef
 [Dt [gt (Tuple Integer Integer)]]
 (Tuple Bool (LM (Tuple Integer Integer) Bool))
 ((Tuple Integer Integer)))
(def
 [fwd [gt (Tuple Integer Integer)]] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [gt (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
 (tuple (tuple) (tuple)))
(edef [lt (Tuple Float Float)] Bool ((Tuple Float Float)))
(edef
 [D [lt (Tuple Float Float)]]
 (LM (Tuple Float Float) Bool)
 ((Tuple Float Float)))
(edef
 [Dt [lt (Tuple Float Float)]]
 (Tuple Bool (LM (Tuple Float Float) Bool))
 ((Tuple Float Float)))
(def
 [fwd [lt (Tuple Float Float)]] (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (tuple))
(def
 [rev [lt (Tuple Float Float)]] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
 (tuple 0.0 0.0))
(edef [lt (Tuple Integer Integer)] Bool ((Tuple Integer Integer)))
(edef
 [D [lt (Tuple Integer Integer)]]
 (LM (Tuple Integer Integer) Bool)
 ((Tuple Integer Integer)))
(edef
 [Dt [lt (Tuple Integer Integer)]]
 (Tuple Bool (LM (Tuple Integer Integer) Bool))
 ((Tuple Integer Integer)))
(def
 [fwd [lt (Tuple Integer Integer)]] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [lt (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
 (tuple (tuple) (tuple)))
(edef [lte (Tuple Float Float)] Bool ((Tuple Float Float)))
(edef
 [D [lte (Tuple Float Float)]]
 (LM (Tuple Float Float) Bool)
 ((Tuple Float Float)))
(edef
 [Dt [lte (Tuple Float Float)]]
 (Tuple Bool (LM (Tuple Float Float) Bool))
 ((Tuple Float Float)))
(def
 [fwd [lte (Tuple Float Float)]] (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (tuple))
(def
 [rev [lte (Tuple Float Float)]] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
 (tuple 0.0 0.0))
(edef [lte (Tuple Integer Integer)] Bool ((Tuple Integer Integer)))
(edef
 [D [lte (Tuple Integer Integer)]]
 (LM (Tuple Integer Integer) Bool)
 ((Tuple Integer Integer)))
(edef
 [Dt [lte (Tuple Integer Integer)]]
 (Tuple Bool (LM (Tuple Integer Integer) Bool))
 ((Tuple Integer Integer)))
(def
 [fwd [lte (Tuple Integer Integer)]] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [lte (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
 (tuple (tuple) (tuple)))
(edef [gte (Tuple Float Float)] Bool ((Tuple Float Float)))
(edef
 [D [gte (Tuple Float Float)]]
 (LM (Tuple Float Float) Bool)
 ((Tuple Float Float)))
(edef
 [Dt [gte (Tuple Float Float)]]
 (Tuple Bool (LM (Tuple Float Float) Bool))
 ((Tuple Float Float)))
(def
 [fwd [gte (Tuple Float Float)]] (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (tuple))
(def
 [rev [gte (Tuple Float Float)]] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
 (tuple 0.0 0.0))
(edef [gte (Tuple Integer Integer)] Bool ((Tuple Integer Integer)))
(edef
 [D [gte (Tuple Integer Integer)]]
 (LM (Tuple Integer Integer) Bool)
 ((Tuple Integer Integer)))
(edef
 [Dt [gte (Tuple Integer Integer)]]
 (Tuple Bool (LM (Tuple Integer Integer) Bool))
 ((Tuple Integer Integer)))
(def
 [fwd [gte (Tuple Integer Integer)]] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [gte (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
 (tuple (tuple) (tuple)))
(edef [log Float] Float (Float))
(edef [D [log Float]] (LM Float Float) (Float))
(def
 [fwd [log Float]] Float
 ((x : Float) (dx : Float))
 ([div (Tuple Float Float)] dx x))
(def
 [rev [log Float]] Float
 ((x : Float) (d_dlog : Float))
 ([div (Tuple Float Float)] d_dlog x))
(edef [Dt [log Float]] (Tuple Float (LM Float Float)) (Float))
(edef [exp Float] Float (Float))
(edef [D [exp Float]] (LM Float Float) (Float))
(def
 [fwd [exp Float]] Float
 ((x : Float) (dx : Float))
 ([mul (Tuple Float Float)] ([exp Float] x) dx))
(def
 [rev [exp Float]] Float
 ((x : Float) (d_dexp : Float))
 ([mul (Tuple Float Float)] ([exp Float] x) d_dexp))
(edef [Dt [exp Float]] (Tuple Float (LM Float Float)) (Float))
(def
 [exp (Vec Float)] (Vec Float)
 (v : Vec Float)
 (build (size v) (lam (i : Integer) ([exp Float] (index i v)))))
(def
 [fwd [exp (Vec Float)]] (Vec Float)
 (_t1 : (Tuple (Vec Float) (Vec Float)))
 (let
  ((v (get$1$2 _t1))
   (ksc$argVar (get$2$2 _t1)))
  (build (size v)
         (lam (i : Integer) ([fwd [exp Float]] (index i v)
                                               (index i ksc$argVar))))))
(def
 [rev [exp (Vec Float)]] (Vec Float)
 (_t1 : (Tuple (Vec Float) (Vec Float)))
 (let
  ((v (get$1$2 _t1))
   (ksc$argVar (get$2$2 _t1)))
  (build (size v)
         (lam (i : Integer) ([rev [exp Float]] (index i v)
                                               (index i ksc$argVar))))))
(edef [sin Float] Float (Float))
(edef [cos Float] Float (Float))
(edef [D [sin Float]] (LM Float Float) (Float))
(def
 [fwd [sin Float]] Float
 ((x : Float) (dx : Float))
 ([mul (Tuple Float Float)] ([cos Float] x) dx))
(def
 [rev [sin Float]] Float
 ((x : Float) (d_dsin : Float))
 ([mul (Tuple Float Float)] ([cos Float] x) d_dsin))
(edef [Dt [sin Float]] (Tuple Float (LM Float Float)) (Float))
(edef [D [cos Float]] (LM Float Float) (Float))
(def
 [fwd [cos Float]] Float
 ((x : Float) (dx : Float))
 ([neg Float] ([mul (Tuple Float Float)] ([sin Float] x) dx)))
(def
 [rev [cos Float]] Float
 ((x : Float) (d_dcos : Float))
 ([neg Float] ([mul (Tuple Float Float)] ([sin Float] x) d_dcos)))
(edef [Dt [cos Float]] (Tuple Float (LM Float Float)) (Float))
(edef [cosh Float] Float (Float))
(edef [tanh Float] Float (Float))
(def
 [fwd [tanh Float]] Float
 ((x : Float) (dx : Float))
 (let
  ((cosh_x ([cosh Float] x)))
  ([div (Tuple Float Float)] dx
                             ([mul (Tuple Float Float)] cosh_x cosh_x))))
(def
 [rev [tanh Float]] Float
 ((x : Float) (d_dr : Float))
 (let
  ((cosh_x ([cosh Float] x)))
  ([div (Tuple Float Float)] d_dr
                             ([mul (Tuple Float Float)] cosh_x cosh_x))))
(edef [D [tanh Float]] (LM Float Float) (Float))
(edef [Dt [tanh Float]] (Tuple Float (LM Float Float)) (Float))
(edef [max (Tuple Float Float)] Float ((Tuple Float Float)))
(edef
 [D [max (Tuple Float Float)]]
 (LM (Tuple Float Float) Float)
 ((Tuple Float Float)))
(edef
 [Dt [max (Tuple Float Float)]]
 (Tuple Float (LM (Tuple Float Float) Float))
 ((Tuple Float Float)))
(edef [imax (Vec Float)] Integer ((Vec Float)))
(edef [max (Vec Float)] Float ((Vec Float)))
(edef [D [max (Vec Float)]] (LM (Vec Float) Float) ((Vec Float)))
(edef
 [Dt [max (Vec Float)]]
 (Tuple Float (LM (Vec Float) Float))
 ((Vec Float)))
(def
 [fwd [max (Vec Float)]] Float
 ((x : Vec Float) (dx : Vec Float))
 (index ([imax (Vec Float)] x) dx))
(def
 [rev [max (Vec Float)]] (Vec Float)
 ((x : Vec Float) (d_dr : Float))
 (deltaVec (size x) ([imax (Vec Float)] x) d_dr))
(edef [$ranhashdoub Integer] Float (Integer))
(edef [D [$ranhashdoub Integer]] (LM Integer Float) (Integer))
(def
 [fwd [$ranhashdoub Integer]] Float
 ((x : Integer) (dx : (Tuple)))
 0.0)
(def
 [rev [$ranhashdoub Integer]] (Tuple)
 ((x : Integer) (d_dranhashdoub : Float))
 (tuple))
(edef
 [Dt [$ranhashdoub Integer]]
 (Tuple Float (LM Integer Float))
 (Integer))
(edef [abs Float] Float (Float))
(edef [D [abs Float]] (LM Float Float) (Float))
(def
 [fwd [abs Float]] Float
 ((x : Float) (dx : Float))
 (if ([gt (Tuple Float Float)] x 0.0) dx ([neg Float] dx)))
(def
 [rev [abs Float]] Float
 ((x : Float) (d_dabs : Float))
 (if ([gt (Tuple Float Float)] x 0.0) d_dabs ([neg Float] d_dabs)))
(edef [Dt [abs Float]] (Tuple Float (LM Float Float)) (Float))
(edef [lgamma Float] Float (Float))
(edef [D [lgamma Float]] (LM Float Float) (Float))
(edef [fwd [lgamma Float]] Float ((Tuple Float Float)))
(edef [rev [lgamma Float]] Float ((Tuple Float Float)))
(edef [Dt [lgamma Float]] (Tuple Float (LM Float Float)) (Float))
(edef [or (Tuple Bool Bool)] Bool ((Tuple Bool Bool)))
(edef
 [D [or (Tuple Bool Bool)]]
 (LM (Tuple Bool Bool) Bool)
 ((Tuple Bool Bool)))
(edef
 [Dt [or (Tuple Bool Bool)]]
 (Tuple Bool (LM (Tuple Bool Bool) Bool))
 ((Tuple Bool Bool)))
(def
 [fwd [or (Tuple Bool Bool)]] (Tuple)
 ((xt : (Tuple Bool Bool)) (dxt : (Tuple Bool Bool)))
 (tuple))
(def
 [rev [or (Tuple Bool Bool)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Bool Bool)) (d_dbool : (Tuple)))
 (tuple (tuple) (tuple)))
(edef [and (Tuple Bool Bool)] Bool ((Tuple Bool Bool)))
(edef
 [D [and (Tuple Bool Bool)]]
 (LM (Tuple Bool Bool) Bool)
 ((Tuple Bool Bool)))
(edef
 [Dt [and (Tuple Bool Bool)]]
 (Tuple Bool (LM (Tuple Bool Bool) Bool))
 ((Tuple Bool Bool)))
(def
 [fwd [and (Tuple Bool Bool)]] (Tuple)
 ((xt : (Tuple Bool Bool)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple))
(def
 [rev [and (Tuple Bool Bool)]] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Bool Bool)) (d_dbool : (Tuple)))
 (tuple (tuple) (tuple)))
(def
 [gmm_knossos_tri Integer] Integer
 (n : Integer)
 ([div (Tuple Integer Integer)] ([mul (Tuple Integer Integer)] n
                                                               ([sub (Tuple Integer Integer)] n 1))
                                2))
(def
 [fwd [gmm_knossos_tri Integer]] (Tuple)
 (_t1 : (Tuple Integer (Tuple)))
 (let
  ((n (get$1$2 _t1))
   (t$5094 ([sub (Tuple Integer Integer)] n 1))
   (t$5099 (get$2$2 _t1)))
  ([fwd [div (Tuple Integer Integer)]] (tuple ([mul (Tuple Integer
                                                           Integer)] n t$5094)
                                              2)
                                       (tuple ([fwd [mul (Tuple Integer Integer)]] (tuple n t$5094)
                                                                                   (tuple t$5099
                                                                                          ([fwd [sub (Tuple Integer
                                                                                                            Integer)]] (tuple n
                                                                                                                              1)
                                                                                                                       (tuple t$5099
                                                                                                                              (tuple)))))
                                              (tuple)))))
(def
 [rev [gmm_knossos_tri Integer]] (Tuple)
 (_t1 : (Tuple Integer (Tuple)))
 (tuple))
(edef
 [dot (Tuple (Vec Float) (Vec Float))]
 Float
 ((Tuple (Vec Float) (Vec Float))))
(edef
 [D [dot (Tuple (Vec Float) (Vec Float))]]
 (LM (Tuple (Vec Float) (Vec Float)) Float)
 ((Tuple (Vec Float) (Vec Float))))
(edef
 [Dt [dot (Tuple (Vec Float) (Vec Float))]]
 (Tuple Float (LM (Tuple (Vec Float) (Vec Float)) Float))
 ((Tuple (Vec Float) (Vec Float))))
(edef
 [R$dot (Tuple (Vec Float) (Vec Float))]
 (LM Float (Tuple (Vec Float) (Vec Float)))
 ((Tuple (Vec Float) (Vec Float))))
(def
 [fwd [dot (Tuple (Vec Float) (Vec Float))]] Float
 ((a_b : (Tuple (Vec Float) (Vec Float)))
  (da_db : (Tuple (Vec Float) (Vec Float))))
 (let
  (((a b) a_b)
   ((da db) da_db))
  ([add (Tuple Float Float)] ([dot (Tuple (Vec Float) (Vec Float))] a
                                                                    db)
                             ([dot (Tuple (Vec Float) (Vec Float))] da b))))
(def
 [rev [dot (Tuple (Vec Float) (Vec Float))]] (Tuple (Vec Float)
                                                    (Vec Float))
 ((a_b : (Tuple (Vec Float) (Vec Float))) (dr : Float))
 (let
  (((a b) a_b))
  (tuple ([mul (Tuple Float (Vec Float))] dr b)
         ([mul (Tuple Float (Vec Float))] dr a))))
(def
 [dot (Tuple (Vec (Vec Float)) (Vec (Vec Float)))] Float
 ((a : Vec (Vec Float)) (b : Vec (Vec Float)))
 (sumbuild (size a)
           (lam (i : Integer) ([dot (Tuple (Vec Float) (Vec Float))] (index i
                                                                            a)
                                                                     (index i b)))))
(def
 [sqnorm (Vec Float)] Float
 (v : Vec Float)
 ([dot (Tuple (Vec Float) (Vec Float))] v v))
(def
 [fwd [sqnorm (Vec Float)]] Float
 (_t1 : (Tuple (Vec Float) (Vec Float)))
 (let
  ((v (get$1$2 _t1))
   (t$5117 (get$2$2 _t1)))
  ([fwd [dot (Tuple (Vec Float) (Vec Float))]] (tuple v v)
                                               (tuple t$5117 t$5117))))
(def
 [rev [sqnorm (Vec Float)]] (Vec Float)
 (_t1 : (Tuple (Vec Float) Float))
 (let
  ((v (get$1$2 _t1))
   (t$5122 ([rev [dot (Tuple (Vec Float) (Vec Float))]] (tuple v v)
                                                        (get$2$2 _t1))))
  (ts_add (get$1$2 t$5122) (get$2$2 t$5122))))
(def
 [gmm_knossos_makeQ (Tuple (Vec Float)
                           (Vec Float))] (Tensor 2 Float)
 ((q : Vec Float) (l : Vec Float))
 (let
  ((D (size q))
   (t$5129 ([gmm_knossos_tri Integer] D)))
  (assert (eq (size l) t$5129)
   (build (tuple D D)
          (lam (ij : (Tuple Integer Integer)) (let
                                               (((i j) ij))
                                               (if
                                                ([lt (Tuple Integer Integer)] i j)
                                                0.0
                                                (if
                                                 (eq i j)
                                                 ([exp Float] (index i q))
                                                 (index ([add (Tuple Integer
                                                                     Integer)] ([sub (Tuple Integer
                                                                                            Integer)] t$5129
                                                                                                      ([gmm_knossos_tri Integer] ([sub (Tuple Integer
                                                                                                                                              Integer)] D
                                                                                                                                                        j)))
                                                                               ([sub (Tuple Integer
                                                                                            Integer)] ([sub (Tuple Integer
                                                                                                                   Integer)] i
                                                                                                                             j)
                                                                                                      1))
                                                        l)))))))))
(def
 [fwd [gmm_knossos_makeQ (Tuple (Vec Float)
                                (Vec Float))]] (Tensor 2 Float)
 (_t1 : (Tuple (Tuple (Vec Float) (Vec Float))
               (Tuple (Vec Float) (Vec Float))))
 (let
  ((_t (get$1$2 _t1))
   (q (get$1$2 _t))
   (D (size q))
   (t$5141 ([gmm_knossos_tri Integer] D)))
  (assert (eq (size (get$2$2 _t)) t$5141)
   (let
    ((ksc$argVar (get$2$2 _t1)))
    (build (tuple D D)
           (lam (ij : (Tuple Integer Integer)) (let
                                                ((i (get$1$2 ij))
                                                 (j (get$2$2 ij)))
                                                (if
                                                 ([lt (Tuple Integer Integer)] i j)
                                                 0.0
                                                 (if
                                                  (eq i j)
                                                  ([fwd [exp Float]] (index i q)
                                                                     (index i (get$1$2 ksc$argVar)))
                                                  (index ([add (Tuple Integer
                                                                      Integer)] ([sub (Tuple Integer
                                                                                             Integer)] t$5141
                                                                                                       ([gmm_knossos_tri Integer] ([sub (Tuple Integer
                                                                                                                                               Integer)] D
                                                                                                                                                         j)))
                                                                                ([sub (Tuple Integer
                                                                                             Integer)] ([sub (Tuple Integer
                                                                                                                    Integer)] i
                                                                                                                              j)
                                                                                                       1))
                                                         (get$2$2 ksc$argVar)))))))))))
(def
 [rev [gmm_knossos_makeQ (Tuple (Vec Float)
                                (Vec Float))]] (Tuple (Vec Float) (Vec Float))
 (_t1 : (Tuple (Tuple (Vec Float) (Vec Float)) (Tensor 2 Float)))
 (let
  ((_t (get$1$2 _t1))
   (q (get$1$2 _t))
   (D (size q))
   (t$5155 ([gmm_knossos_tri Integer] D)))
  (assert (eq (size (get$2$2 _t)) t$5155)
   (let
    ((ksc$argVar (get$2$2 _t1)))
    (tuple
       (build D (lam (i : Integer) ([rev [exp Float]] (index i q) (index (tuple i i) ksc$argVar))))
    (buildFromSparse (constVec t$5155 (tuple)) (tuple D D)
              (lam (ij : (Tuple Integer Integer)) (let
                                                   ((i j) ij)
                                                   (if
                                                    ([lte (Tuple Integer Integer)] i j)
                                                    (tuple 0 0.0)
                                                            (tuple
                                                                      ([add (Tuple Integer
                                                                                   Integer)] ([sub (Tuple Integer
                                                                                                          Integer)] t$5155
                                                                                                                    ([gmm_knossos_tri Integer] ([sub (Tuple Integer
                                                                                                                                                            Integer)] D
                                                                                                                                                                      j)))
                                                                                             ([sub (Tuple Integer
                                                                                                          Integer)] ([sub (Tuple Integer
                                                                                                                                 Integer)] i
                                                                                                                                           j)
                                                                                                                    1))
                                                                      (index ij
                                                                             ksc$argVar)))))))))))
(def
 [logsumexp (Vec Float)] Float
 (v : Vec Float)
 (let
  ((maxv ([max (Vec Float)] v)))
  ([add (Tuple Float Float)] maxv
                             ([log Float] (sum ([exp (Vec Float)] ([sub (Tuple (Vec Float)
                                                                               Float)] v maxv)))))))
(def
 [rev [logsumexp (Vec Float)]] (Vec Float)
 (_t1 : (Tuple (Vec Float) Float))
 (let
  ((v (get$1$2 _t1))
   (maxv ([max (Vec Float)] v))
   (t$5188 ([sub (Tuple (Vec Float) Float)] v maxv))
   (t$5189 ([exp (Vec Float)] t$5188))
   (t$5190 (sum t$5189))
   (t$5194 ([rev [add (Tuple Float Float)]] (tuple maxv
                                                   ([log Float] t$5190))
                                            (get$2$2 _t1)))
   (t$5216 ([rev [sub (Tuple (Vec Float) Float)]] (tuple v maxv)
                                                  ([rev [exp (Vec Float)]] t$5188
                                                                           (constVec (size t$5189)
                                                                                     ([rev [log Float]] t$5190
                                                                                                        (get$2$2 t$5194)))))))
  (ts_add ([rev [max (Vec Float)]] v (get$1$2 t$5194))
          (ts_add (get$1$2 t$5216)
                  ([rev [max (Vec Float)]] v (get$2$2 t$5216))))))
(def
 [fwd [logsumexp (Vec Float)]] Float
 (_t1 : (Tuple (Vec Float) (Vec Float)))
 (let
  ((v (get$1$2 _t1))
   (maxv ([max (Vec Float)] v))
   (t$5241 ([sub (Tuple (Vec Float) Float)] v maxv))
   (t$5243 (get$2$2 _t1))
   (t$5245 ([fwd [max (Vec Float)]] v t$5243))
   (ksc$argVar ([fwd [exp (Vec Float)]] t$5241
                                        ([fwd [sub (Tuple (Vec Float) Float)]] (tuple v maxv)
                                                                               (tuple t$5243
                                                                                      t$5245))))
   (t$5249 ([exp (Vec Float)] t$5241))
   (t$5250 (sum t$5249)))
  ([fwd [add (Tuple Float Float)]] (tuple maxv ([log Float] t$5250))
                                   (tuple t$5245
                                          ([fwd [log Float]] t$5250
                                                             (sumbuild (size t$5249)
                                                                       (lam (ksc$indexTVar : Integer) (index ksc$indexTVar
                                                                                                             ksc$argVar))))))))
(def
 [log_gamma_distrib (Tuple Float Integer)] Float
 ((a : Float) (p : Integer))
 ([add (Tuple Float Float)] ([mul (Tuple Float
                                         Float)] 0.28618247146235004
                                                 ([to_float Integer] ([mul (Tuple Integer
                                                                                  Integer)] p
                                                                                            ([sub (Tuple Integer
                                                                                                         Integer)] p
                                                                                                                   1))))
                            (sumbuild p
                                      (lam (j : Integer) ([lgamma Float] ([sub (Tuple Float
                                                                                      Float)] a
                                                                                              ([mul (Tuple Float
                                                                                                           Float)] 0.5
                                                                                                                   ([to_float Integer] j))))))))
(def
 [fwd [log_gamma_distrib (Tuple Float Integer)]] Float
 (_t1 : (Tuple (Tuple Float Integer) (Tuple Float (Tuple))))
 (let
  ((_t (get$1$2 _t1))
   (a (get$1$2 _t))
   (p (get$2$2 _t))
   (ksc$argVar (get$2$2 _t1))
   (ksc$argVar_5 (build p
                        (lam (j : Integer) (let
                                            ((t$5272 ([to_float Integer] j))
                                             (t$5273 ([mul (Tuple Float Float)] 0.5 t$5272)))
                                            ([fwd [lgamma Float]] ([sub (Tuple Float Float)] a
                                                                                             t$5273)
                                                                  ([fwd [sub (Tuple Float
                                                                                    Float)]] (tuple a
                                                                                                    t$5273)
                                                                                             (tuple (get$1$2 ksc$argVar)
                                                                                                    ([fwd [mul (Tuple Float
                                                                                                                      Float)]] (tuple 0.5
                                                                                                                                      t$5272)
                                                                                                                               (tuple 0.0
                                                                                                                                      ([fwd [to_float Integer]] j
                                                                                                                                                                (tuple)))))))))))
   (t$5287 ([sub (Tuple Integer Integer)] p 1))
   (t$5288 ([mul (Tuple Integer Integer)] p t$5287))
   (t$5289 ([to_float Integer] t$5288)))
  ([fwd [add (Tuple Float Float)]] (tuple ([mul (Tuple Float
                                                       Float)] 0.28618247146235004 t$5289)
                                          (sumbuild p
                                                    (lam (j : Integer) ([lgamma Float] ([sub (Tuple Float
                                                                                                    Float)] a
                                                                                                            ([mul (Tuple Float
                                                                                                                         Float)] 0.5
                                                                                                                                 ([to_float Integer] j)))))))
                                   (tuple ([fwd [mul (Tuple Float
                                                            Float)]] (tuple 0.28618247146235004
                                                                            t$5289)
                                                                     (tuple 0.0
                                                                            ([fwd [to_float Integer]] t$5288
                                                                                                      ([fwd [mul (Tuple Integer
                                                                                                                        Integer)]] (tuple p
                                                                                                                                          t$5287)
                                                                                                                                   (tuple (tuple)
                                                                                                                                          ([fwd [sub (Tuple Integer
                                                                                                                                                            Integer)]] (tuple p
                                                                                                                                                                              1)
                                                                                                                                                                       (tuple (tuple)
                                                                                                                                                                              (tuple))))))))
                                          (sumbuild p
                                                    (lam (ksc$indexTVar : Integer) (index ksc$indexTVar
                                                                                          ksc$argVar_5)))))))
(def
 [rev [log_gamma_distrib (Tuple Float Integer)]] (Tuple Float
                                                        (Tuple))
 (_t1 : (Tuple (Tuple Float Integer) Float))
 (let
  ((_t (get$1$2 _t1))
   (a (get$1$2 _t))
   (p (get$2$2 _t))
   (ksc$argVar (constVec p
                         (get$2$2 ([rev [add (Tuple Float Float)]] (tuple ([mul (Tuple Float
                                                                                       Float)] 0.28618247146235004
                                                                                               ([to_float Integer] ([mul (Tuple Integer
                                                                                                                                Integer)] p
                                                                                                                                          ([sub (Tuple Integer
                                                                                                                                                       Integer)] p
                                                                                                                                                                 1))))
                                                                          (sumbuild p
                                                                                    (lam (j : Integer) ([lgamma Float] ([sub (Tuple Float
                                                                                                                                    Float)] a
                                                                                                                                            ([mul (Tuple Float
                                                                                                                                                         Float)] 0.5
                                                                                                                                                                 ([to_float Integer] j)))))))
                                                                   (get$2$2 _t1))))))
  (tuple (sumbuild p
                   (lam (j : Integer) (let
                                       ((t$5330 ([mul (Tuple Float Float)] 0.5
                                                                           ([to_float Integer] j))))
                                       (get$1$2 ([rev [sub (Tuple Float Float)]] (tuple a t$5330)
                                                                                 ([rev [lgamma Float]] ([sub (Tuple Float
                                                                                                                    Float)] a
                                                                                                                            t$5330)
                                                                                                       (index j
                                                                                                              ksc$argVar)))))))
         (tuple))))
(def
 [log_wishart_prior (Tuple (Tuple Float Integer)
                           (Vec Float)
                           (Vec Float))] Float
 ((wishart : (Tuple Float Integer))
  (log_Qdiag : Vec Float)
  (ltri_Q : Vec Float))
 (let
  ((p (size log_Qdiag))
   ((wishart_gamma wishart_m) wishart)
   (n ([add (Tuple Integer Integer)] p
                                     ([add (Tuple Integer Integer)] wishart_m 1))))
  ([sub (Tuple Float Float)] ([sub (Tuple Float
                                          Float)] ([mul (Tuple Float Float)] 0.5
                                                                             ([mul (Tuple Float
                                                                                          Float)] ([mul (Tuple Float
                                                                                                               Float)] wishart_gamma
                                                                                                                       wishart_gamma)
                                                                                                  ([add (Tuple Float
                                                                                                               Float)] ([sqnorm (Vec Float)] ([exp (Vec Float)] log_Qdiag))
                                                                                                                       ([sqnorm (Vec Float)] ltri_Q))))
                                                  ([mul (Tuple Float
                                                               Float)] ([to_float Integer] wishart_m)
                                                                       (sum log_Qdiag)))
                             ([sub (Tuple Float Float)] ([mul (Tuple Float
                                                                     Float)] ([to_float Integer] ([mul (Tuple Integer
                                                                                                              Integer)] n
                                                                                                                        p))
                                                                             ([sub (Tuple Float
                                                                                          Float)] ([log Float] wishart_gamma)
                                                                                                  ([mul (Tuple Float
                                                                                                               Float)] 0.5
                                                                                                                       ([log Float] 2.0))))
                                                        ([log_gamma_distrib (Tuple Float
                                                                                   Integer)] ([mul (Tuple Float
                                                                                                          Float)] 0.5
                                                                                                                  ([to_float Integer] n))
                                                                                             p)))))
(def
 [fwd [log_wishart_prior (Tuple (Tuple Float Integer)
                                (Vec Float)
                                (Vec Float))]] Float
 (_t1 : (Tuple (Tuple (Tuple Float Integer) (Vec Float) (Vec Float))
               (Tuple (Tuple Float (Tuple)) (Vec Float) (Vec Float))))
 (let
  ((_t (get$1$2 _t1))
   (log_Qdiag (get$2$3 _t))
   (ltri_Q (get$3$3 _t))
   (p (size log_Qdiag))
   (_t_5 (get$1$3 _t))
   (wishart_gamma (get$1$2 _t_5))
   (wishart_m (get$2$2 _t_5))
   (sum_qs (sum log_Qdiag))
   (Qdiag ([exp (Vec Float)] log_Qdiag))
   (t$5363 ([add (Tuple Integer Integer)] wishart_m 1))
   (n ([add (Tuple Integer Integer)] p t$5363))
   (t$5364 ([sqnorm (Vec Float)] Qdiag))
   (t$5365 ([sqnorm (Vec Float)] ltri_Q))
   (frobenius ([add (Tuple Float Float)] t$5364 t$5365))
   (t$5366 ([mul (Tuple Float Float)] wishart_gamma wishart_gamma))
   (t$5367 ([mul (Tuple Float Float)] t$5366 frobenius))
   (w2f ([mul (Tuple Float Float)] 0.5 t$5367))
   (t$5368 (get$2$2 _t1))
   (ksc$argVar (get$2$3 t$5368))
   (t$5369 ([to_float Integer] wishart_m))
   (t$5370 ([mul (Tuple Float Float)] t$5369 sum_qs))
   (t$5372 ([mul (Tuple Integer Integer)] n p))
   (t$5373 ([to_float Integer] t$5372))
   (t$5374 ([log Float] wishart_gamma))
   (t$5375 ([log Float] 2.0))
   (t$5376 ([mul (Tuple Float Float)] 0.5 t$5375))
   (t$5377 ([sub (Tuple Float Float)] t$5374 t$5376))
   (t$5378 ([mul (Tuple Float Float)] t$5373 t$5377))
   (t$5379 ([to_float Integer] n))
   (t$5380 ([mul (Tuple Float Float)] 0.5 t$5379))
   (t$5381 ([log_gamma_distrib (Tuple Float Integer)] t$5380 p))
   (t$5394 (get$1$3 t$5368))
   (t$5395 (get$1$2 t$5394))
   (t$5408 ([fwd [sqnorm (Vec Float)]] Qdiag
                                       ([fwd [exp (Vec Float)]] log_Qdiag (constVec p 0.0))))
   (t$5423 ([fwd [sqnorm (Vec Float)]] ltri_Q
                                       (constVec (size ltri_Q) 0.0)))
   (t$5442 ([fwd [to_float Integer]] wishart_m (tuple)))
   (t$5489 ([fwd [add (Tuple Integer Integer)]] (tuple p t$5363)
                                                (tuple (tuple)
                                                       ([fwd [add (Tuple Integer
                                                                         Integer)]] (tuple wishart_m
                                                                                           1)
                                                                                    (tuple (tuple)
                                                                                           (tuple))))))
   (t$5502 ([fwd [log Float]] wishart_gamma 0.0)))
  ([fwd [sub (Tuple Float Float)]] (tuple ([sub (Tuple Float
                                                       Float)] w2f t$5370)
                                          ([sub (Tuple Float Float)] t$5378 t$5381))
                                   (tuple ([fwd [sub (Tuple Float Float)]] (tuple w2f t$5370)
                                                                           (tuple ([fwd [mul (Tuple Float
                                                                                                    Float)]] (tuple 0.5
                                                                                                                    t$5367)
                                                                                                             (tuple 0.0
                                                                                                                    ([fwd [mul (Tuple Float
                                                                                                                                      Float)]] (tuple t$5366
                                                                                                                                                      frobenius)
                                                                                                                                               (tuple ([fwd [mul (Tuple Float
                                                                                                                                                                        Float)]] (tuple wishart_gamma
                                                                                                                                                                                        wishart_gamma)
                                                                                                                                                                                 (tuple t$5395
                                                                                                                                                                                        t$5395))
                                                                                                                                                      ([fwd [add (Tuple Float
                                                                                                                                                                        Float)]] (tuple t$5364
                                                                                                                                                                                        t$5365)
                                                                                                                                                                                 (tuple (ts_add t$5408
                                                                                                                                                                                                (ts_add ([fwd [sqnorm (Vec Float)]] Qdiag
                                                                                                                                                                                                                                    ([fwd [exp (Vec Float)]] log_Qdiag
                                                                                                                                                                                                                                                             ksc$argVar))
                                                                                                                                                                                                        t$5408))
                                                                                                                                                                                        (ts_add t$5423
                                                                                                                                                                                                (ts_add t$5423
                                                                                                                                                                                                        ([fwd [sqnorm (Vec Float)]] ltri_Q
                                                                                                                                                                                                                                    (get$3$3 t$5368))))))))))
                                                                                  ([fwd [mul (Tuple Float
                                                                                                    Float)]] (tuple t$5369
                                                                                                                    sum_qs)
                                                                                                             (tuple (ts_add (ts_add t$5442
                                                                                                                                    ([fwd [to_float Integer]] wishart_m
                                                                                                                                                              (get$2$2 t$5394)))
                                                                                                                            (ts_add t$5442
                                                                                                                                    t$5442))
                                                                                                                    (sumbuild p
                                                                                                                              (lam (ksc$indexTVar : Integer) (index ksc$indexTVar
                                                                                                                                                                    ksc$argVar)))))))
                                          ([fwd [sub (Tuple Float Float)]] (tuple t$5378 t$5381)
                                                                           (tuple ([fwd [mul (Tuple Float
                                                                                                    Float)]] (tuple t$5373
                                                                                                                    t$5377)
                                                                                                             (tuple ([fwd [to_float Integer]] t$5372
                                                                                                                                              ([fwd [mul (Tuple Integer
                                                                                                                                                                Integer)]] (tuple n
                                                                                                                                                                                  p)
                                                                                                                                                                           (tuple t$5489
                                                                                                                                                                                  (tuple))))
                                                                                                                    ([fwd [sub (Tuple Float
                                                                                                                                      Float)]] (tuple t$5374
                                                                                                                                                      t$5376)
                                                                                                                                               (tuple (ts_add (ts_add ([fwd [log Float]] wishart_gamma
                                                                                                                                                                                         t$5395)
                                                                                                                                                                      t$5502)
                                                                                                                                                              (ts_add t$5502
                                                                                                                                                                      t$5502))
                                                                                                                                                      ([fwd [mul (Tuple Float
                                                                                                                                                                        Float)]] (tuple 0.5
                                                                                                                                                                                        t$5375)
                                                                                                                                                                                 (tuple 0.0
                                                                                                                                                                                        ([fwd [log Float]] 2.0
                                                                                                                                                                                                           0.0)))))))
                                                                                  ([fwd [log_gamma_distrib (Tuple Float
                                                                                                                  Integer)]] (tuple t$5380
                                                                                                                                    p)
                                                                                                                             (tuple ([fwd [mul (Tuple Float
                                                                                                                                                      Float)]] (tuple 0.5
                                                                                                                                                                      t$5379)
                                                                                                                                                               (tuple 0.0
                                                                                                                                                                      ([fwd [to_float Integer]] n
                                                                                                                                                                                                t$5489)))
                                                                                                                                    (tuple)))))))))
(def
 [rev [log_wishart_prior (Tuple (Tuple Float Integer)
                                (Vec Float)
                                (Vec Float))]] (Tuple (Tuple Float (Tuple))
                                                      (Vec Float)
                                                      (Vec Float))
 (_t1 : (Tuple (Tuple (Tuple Float Integer) (Vec Float) (Vec Float))
               Float))
 (let
  ((_t (get$1$2 _t1))
   (log_Qdiag (get$2$3 _t))
   (ltri_Q (get$3$3 _t))
   (p (size log_Qdiag))
   (_t_5 (get$1$3 _t))
   (wishart_gamma (get$1$2 _t_5))
   (wishart_m (get$2$2 _t_5))
   (sum_qs (sum log_Qdiag))
   (Qdiag ([exp (Vec Float)] log_Qdiag))
   (n ([add (Tuple Integer Integer)] p
                                     ([add (Tuple Integer Integer)] wishart_m 1)))
   (t$5542 ([sqnorm (Vec Float)] Qdiag))
   (t$5543 ([sqnorm (Vec Float)] ltri_Q))
   (frobenius ([add (Tuple Float Float)] t$5542 t$5543))
   (t$5544 ([mul (Tuple Float Float)] wishart_gamma wishart_gamma))
   (t$5545 ([mul (Tuple Float Float)] t$5544 frobenius))
   (w2f ([mul (Tuple Float Float)] 0.5 t$5545))
   (t$5552 ([to_float Integer] wishart_m))
   (t$5553 ([mul (Tuple Float Float)] t$5552 sum_qs))
   (t$5559 ([to_float Integer] ([mul (Tuple Integer Integer)] n p)))
   (t$5560 ([log Float] wishart_gamma))
   (t$5562 ([mul (Tuple Float Float)] 0.5 ([log Float] 2.0)))
   (t$5563 ([sub (Tuple Float Float)] t$5560 t$5562))
   (t$5564 ([mul (Tuple Float Float)] t$5559 t$5563))
   (t$5567 ([log_gamma_distrib (Tuple Float
                                      Integer)] ([mul (Tuple Float Float)] 0.5
                                                                           ([to_float Integer] n))
                                                p))
   (t$5571 ([rev [sub (Tuple Float Float)]] (tuple ([sub (Tuple Float
                                                                Float)] w2f t$5553)
                                                   ([sub (Tuple Float Float)] t$5564 t$5567))
                                            (get$2$2 _t1)))
   (t$5573 ([rev [sub (Tuple Float Float)]] (tuple w2f t$5553)
                                            (get$1$2 t$5571)))
   (t$5577 ([rev [mul (Tuple Float Float)]] (tuple t$5544 frobenius)
                                            (get$2$2 ([rev [mul (Tuple Float Float)]] (tuple 0.5
                                                                                             t$5545)
                                                                                      (get$1$2 t$5573)))))
   (t$5579 ([rev [mul (Tuple Float Float)]] (tuple wishart_gamma
                                                   wishart_gamma)
                                            (get$1$2 t$5577)))
   (t$5702 ([rev [add (Tuple Float Float)]] (tuple t$5542 t$5543)
                                            (get$2$2 t$5577))))
  (tuple (tuple (ts_add (ts_add (get$1$2 t$5579) (get$2$2 t$5579))
                        ([rev [log Float]] wishart_gamma
                                           (get$1$2 ([rev [sub (Tuple Float Float)]] (tuple t$5560
                                                                                            t$5562)
                                                                                     (get$2$2 ([rev [mul (Tuple Float
                                                                                                                Float)]] (tuple t$5559
                                                                                                                                t$5563)
                                                                                                                         (get$1$2 ([rev [sub (Tuple Float
                                                                                                                                                    Float)]] (tuple t$5564
                                                                                                                                                                    t$5567)
                                                                                                                                                             (get$2$2 t$5571)))))))))
                (tuple))
         (ts_add ([rev [exp (Vec Float)]] log_Qdiag
                                          ([rev [sqnorm (Vec Float)]] Qdiag (get$1$2 t$5702)))
                 (constVec p
                           (get$2$2 ([rev [mul (Tuple Float Float)]] (tuple t$5552 sum_qs)
                                                                     (get$2$2 t$5573)))))
         ([rev [sqnorm (Vec Float)]] ltri_Q (get$2$2 t$5702)))))
(def
 [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                   (Vec Float)
                                   (Vec (Vec Float))
                                   (Vec (Vec Float))
                                   (Vec (Vec Float))
                                   (Tuple Float Integer))] Float
 ((x : Vec (Vec Float))
  (alphas : Vec Float)
  (means : Vec (Vec Float))
  (qs : Vec (Vec Float))
  (ls : Vec (Vec Float))
  (wishart : (Tuple Float Integer)))
 (let
  ((N (size x))
   (K (size alphas))
   (D (size (index 0 x))))
  (assert (eq (size (index 0 ls)) ([gmm_knossos_tri Integer] D))
   ([add (Tuple Float Float)] ([add (Tuple Float
                                           Float)] ([mul (Tuple Float
                                                                Float)] ([to_float Integer] ([mul (Tuple Integer
                                                                                                         Integer)] N
                                                                                                                   D))
                                                                        ([neg Float] 0.9189385332046727))
                                                   ([sub (Tuple Float Float)] (sumbuild N
                                                                                        (lam (i : Integer) ([logsumexp (Vec Float)] (build K
                                                                                                                                           (lam (k : Integer) (let
                                                                                                                                                               ((t$5783 (index k
                                                                                                                                                                               qs)))
                                                                                                                                                               ([sub (Tuple Float
                                                                                                                                                                            Float)] ([add (Tuple Float
                                                                                                                                                                                                 Float)] (index k
                                                                                                                                                                                                                alphas)
                                                                                                                                                                                                         (sum t$5783))
                                                                                                                                                                                    ([mul (Tuple Float
                                                                                                                                                                                                 Float)] 0.5
                                                                                                                                                                                                         ([sqnorm (Vec Float)] ([mul (Tuple (Tensor 2 Float)
                                                                                                                                                                                                                                            (Vec Float))] ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                                                                                     (Vec Float))] t$5783
                                                                                                                                                                                                                                                                                                   (index k
                                                                                                                                                                                                                                                                                                          ls))
                                                                                                                                                                                                                                                          ([sub (Tuple (Vec Float)
                                                                                                                                                                                                                                                                       (Vec Float))] (index i
                                                                                                                                                                                                                                                                                            x)
                                                                                                                                                                                                                                                                                     (index k
                                                                                                                                                                                                                                                                                            means))))))))))))
                                                                              ([mul (Tuple Float
                                                                                           Float)] ([to_float Integer] N)
                                                                                                   ([logsumexp (Vec Float)] alphas))))
                              (sumbuild K
                                        (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                             Integer)
                                                                                      (Vec Float)
                                                                                      (Vec Float))] wishart
                                                                                                    (index k
                                                                                                           qs)
                                                                                                    (index k
                                                                                                           ls))))))))
(def
 [fwd [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                        (Vec Float)
                                        (Vec (Vec Float))
                                        (Vec (Vec Float))
                                        (Vec (Vec Float))
                                        (Tuple Float Integer))]] Float
 (_t1 : (Tuple (Tuple (Vec (Vec Float))
                      (Vec Float)
                      (Vec (Vec Float))
                      (Vec (Vec Float))
                      (Vec (Vec Float))
                      (Tuple Float Integer))
               (Tuple (Vec (Vec Float))
                      (Vec Float)
                      (Vec (Vec Float))
                      (Vec (Vec Float))
                      (Vec (Vec Float))
                      (Tuple Float (Tuple)))))
 (let
  ((_t (get$1$2 _t1))
   (x (get$1$6 _t))
   (alphas (get$2$6 _t))
   (means (get$3$6 _t))
   (qs (get$4$6 _t))
   (ls (get$5$6 _t))
   (wishart (get$6$6 _t))
   (N (size x))
   (K (size alphas))
   (D (size (index 0 x))))
  (assert (eq (size (index 0 ls)) ([gmm_knossos_tri Integer] D))
   (let
    ((t$5809 ([mul (Tuple Integer Integer)] N D))
     (t$5810 ([to_float Integer] t$5809))
     (t$5811 ([neg Float] 0.9189385332046727))
     (CONSTANT ([mul (Tuple Float Float)] t$5810 t$5811))
     (slse (sumbuild N
                     (lam (i : Integer) ([logsumexp (Vec Float)] (build K
                                                                        (lam (k : Integer) (let
                                                                                            ((t$5813 (index k
                                                                                                            qs)))
                                                                                            ([sub (Tuple Float
                                                                                                         Float)] ([add (Tuple Float
                                                                                                                              Float)] (index k
                                                                                                                                             alphas)
                                                                                                                                      (sum t$5813))
                                                                                                                 ([mul (Tuple Float
                                                                                                                              Float)] 0.5
                                                                                                                                      ([sqnorm (Vec Float)] ([mul (Tuple (Tensor 2 Float)
                                                                                                                                                                         (Vec Float))] ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                  (Vec Float))] t$5813
                                                                                                                                                                                                                                (index k
                                                                                                                                                                                                                                       ls))
                                                                                                                                                                                       ([sub (Tuple (Vec Float)
                                                                                                                                                                                                    (Vec Float))] (index i
                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                  (index k
                                                                                                                                                                                                                         means)))))))))))))
     (ksc$argVar (get$2$2 _t1))
     (ksc$argVar_14 (build N
                           (lam (i : Integer) ([fwd [logsumexp (Vec Float)]] (build K
                                                                                    (lam (k : Integer) (let
                                                                                                        ((t$5827 (index k
                                                                                                                        qs)))
                                                                                                        ([sub (Tuple Float
                                                                                                                     Float)] ([add (Tuple Float
                                                                                                                                          Float)] (index k
                                                                                                                                                         alphas)
                                                                                                                                                  (sum t$5827))
                                                                                                                             ([mul (Tuple Float
                                                                                                                                          Float)] 0.5
                                                                                                                                                  ([sqnorm (Vec Float)] ([mul (Tuple (Tensor 2 Float)
                                                                                                                                                                                     (Vec Float))] ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                              (Vec Float))] t$5827
                                                                                                                                                                                                                                            (index k
                                                                                                                                                                                                                                                   ls))
                                                                                                                                                                                                   ([sub (Tuple (Vec Float)
                                                                                                                                                                                                                (Vec Float))] (index i
                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                              (index k
                                                                                                                                                                                                                                     means)))))))))
                                                                             (build K
                                                                                    (lam (k : Integer) (let
                                                                                                        ((t$5840 (index k
                                                                                                                        qs))
                                                                                                         (t$5841 (index k
                                                                                                                        ls))
                                                                                                         (Q ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                       (Vec Float))] t$5840
                                                                                                                                                     t$5841))
                                                                                                         (t$5842 (index i
                                                                                                                        x))
                                                                                                         (t$5843 (index k
                                                                                                                        means))
                                                                                                         (t$5844 ([sub (Tuple (Vec Float)
                                                                                                                              (Vec Float))] t$5842
                                                                                                                                            t$5843))
                                                                                                         (mahal_vec ([mul (Tuple (Tensor 2 Float)
                                                                                                                                 (Vec Float))] Q
                                                                                                                                               t$5844))
                                                                                                         (ksc$argVar_25 (index k
                                                                                                                               (get$4$6 ksc$argVar)))
                                                                                                         (t$5871 (index k
                                                                                                                        alphas))
                                                                                                         (t$5873 (sum t$5840))
                                                                                                         (t$5875 ([sqnorm (Vec Float)] mahal_vec)))
                                                                                                        ([fwd [sub (Tuple Float
                                                                                                                          Float)]] (tuple ([add (Tuple Float
                                                                                                                                                       Float)] t$5871
                                                                                                                                                               t$5873)
                                                                                                                                          ([mul (Tuple Float
                                                                                                                                                       Float)] 0.5
                                                                                                                                                               t$5875))
                                                                                                                                   (tuple ([fwd [add (Tuple Float
                                                                                                                                                            Float)]] (tuple t$5871
                                                                                                                                                                            t$5873)
                                                                                                                                                                     (tuple (index k
                                                                                                                                                                                   (get$2$6 ksc$argVar))
                                                                                                                                                                            (sumbuild (size t$5840)
                                                                                                                                                                                      (lam (ksc$indexTVar : Integer) (index ksc$indexTVar
                                                                                                                                                                                                                            ksc$argVar_25)))))
                                                                                                                                          ([fwd [mul (Tuple Float
                                                                                                                                                            Float)]] (tuple 0.5
                                                                                                                                                                            t$5875)
                                                                                                                                                                     (tuple 0.0
                                                                                                                                                                            ([fwd [sqnorm (Vec Float)]] mahal_vec
                                                                                                                                                                                                        ([fwd [mul (Tuple (Tensor 2 Float)
                                                                                                                                                                                                                          (Vec Float))]] (tuple Q
                                                                                                                                                                                                                                                t$5844)
                                                                                                                                                                                                                                         (tuple ([fwd [gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                                                                                (Vec Float))]] (tuple t$5840
                                                                                                                                                                                                                                                                                                      t$5841)
                                                                                                                                                                                                                                                                                               (tuple ksc$argVar_25
                                                                                                                                                                                                                                                                                                      (index k
                                                                                                                                                                                                                                                                                                             (get$5$6 ksc$argVar))))
                                                                                                                                                                                                                                                ([fwd [sub (Tuple (Vec Float)
                                                                                                                                                                                                                                                                  (Vec Float))]] (tuple t$5842
                                                                                                                                                                                                                                                                                        t$5843)
                                                                                                                                                                                                                                                                                 (tuple (index i
                                                                                                                                                                                                                                                                                               (get$1$6 ksc$argVar))
                                                                                                                                                                                                                                                                                        (index k
                                                                                                                                                                                                                                                                                               (get$3$6 ksc$argVar))))))))))))))))))
     (ksc$argVar_16 (build K
                           (lam (k : Integer) ([fwd [log_wishart_prior (Tuple (Tuple Float
                                                                                     Integer)
                                                                              (Vec Float)
                                                                              (Vec Float))]] (tuple wishart
                                                                                                    (index k
                                                                                                           qs)
                                                                                                    (index k
                                                                                                           ls))
                                                                                             (tuple (get$6$6 ksc$argVar)
                                                                                                    (index k
                                                                                                           (get$4$6 ksc$argVar))
                                                                                                    (index k
                                                                                                           (get$5$6 ksc$argVar)))))))
     (t$6079 ([to_float Integer] N))
     (t$6080 ([logsumexp (Vec Float)] alphas))
     (t$6081 ([mul (Tuple Float Float)] t$6079 t$6080))
     (t$6082 ([sub (Tuple Float Float)] slse t$6081))
     (t$6116 ([fwd [to_float Integer]] N (tuple)))
     (t$6135 ([fwd [logsumexp (Vec Float)]] alphas (constVec K 0.0))))
    ([fwd [add (Tuple Float Float)]] (tuple ([add (Tuple Float
                                                         Float)] CONSTANT t$6082)
                                            (sumbuild K
                                                      (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                                           Integer)
                                                                                                    (Vec Float)
                                                                                                    (Vec Float))] wishart
                                                                                                                  (index k
                                                                                                                         qs)
                                                                                                                  (index k
                                                                                                                         ls)))))
                                     (tuple ([fwd [add (Tuple Float Float)]] (tuple CONSTANT t$6082)
                                                                             (tuple ([fwd [mul (Tuple Float
                                                                                                      Float)]] (tuple t$5810
                                                                                                                      t$5811)
                                                                                                               (tuple ([fwd [to_float Integer]] t$5809
                                                                                                                                                ([fwd [mul (Tuple Integer
                                                                                                                                                                  Integer)]] (tuple N
                                                                                                                                                                                    D)
                                                                                                                                                                             (tuple (tuple)
                                                                                                                                                                                    (tuple))))
                                                                                                                      ([fwd [neg Float]] 0.9189385332046727
                                                                                                                                         0.0)))
                                                                                    ([fwd [sub (Tuple Float
                                                                                                      Float)]] (tuple slse
                                                                                                                      t$6081)
                                                                                                               (tuple (sumbuild N
                                                                                                                                (lam (ksc$indexTVar : Integer) (index ksc$indexTVar
                                                                                                                                                                      ksc$argVar_14)))
                                                                                                                      ([fwd [mul (Tuple Float
                                                                                                                                        Float)]] (tuple t$6079
                                                                                                                                                        t$6080)
                                                                                                                                                 (tuple (ts_add t$6116
                                                                                                                                                                (ts_add t$6116
                                                                                                                                                                        (ts_add t$6116
                                                                                                                                                                                (ts_add t$6116
                                                                                                                                                                                        (ts_add t$6116
                                                                                                                                                                                                t$6116)))))
                                                                                                                                                        (ts_add t$6135
                                                                                                                                                                (ts_add ([fwd [logsumexp (Vec Float)]] alphas
                                                                                                                                                                                                       (get$2$6 ksc$argVar))
                                                                                                                                                                        (ts_add t$6135
                                                                                                                                                                                (ts_add t$6135
                                                                                                                                                                                        (ts_add t$6135
                                                                                                                                                                                                t$6135)))))))))))
                                            (sumbuild K
                                                      (lam (ksc$indexTVar : Integer) (index ksc$indexTVar
                                                                                            ksc$argVar_16)))))))))
(def
 [rev [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                        (Vec Float)
                                        (Vec (Vec Float))
                                        (Vec (Vec Float))
                                        (Vec (Vec Float))
                                        (Tuple Float Integer))]] (Tuple (Vec (Vec Float))
                                                                        (Vec Float)
                                                                        (Vec (Vec Float))
                                                                        (Vec (Vec Float))
                                                                        (Vec (Vec Float))
                                                                        (Tuple Float (Tuple)))
 (_t1 : (Tuple (Tuple (Vec (Vec Float))
                      (Vec Float)
                      (Vec (Vec Float))
                      (Vec (Vec Float))
                      (Vec (Vec Float))
                      (Tuple Float Integer))
               Float))
 (let
  ((_t (get$1$2 _t1))
   (x (get$1$6 _t))
   (alphas (get$2$6 _t))
   (means (get$3$6 _t))
   (qs (get$4$6 _t))
   (ls (get$5$6 _t))
   (wishart (get$6$6 _t))
   (N (size x))
   (K (size alphas))
   (D (size (index 0 x)))
   (t$6171 ([gmm_knossos_tri Integer] D)))
  (assert (eq (size (index 0 ls)) t$6171)
   (let
    ((CONSTANT ([mul (Tuple Float
                            Float)] ([to_float Integer] ([mul (Tuple Integer Integer)] N D))
                                    ([neg Float] 0.9189385332046727)))
     (slse (sumbuild N
                     (lam (i : Integer) ([logsumexp (Vec Float)] (build K
                                                                        (lam (k : Integer) (let
                                                                                            ((t$6176 (index k
                                                                                                            qs)))
                                                                                            ([sub (Tuple Float
                                                                                                         Float)] ([add (Tuple Float
                                                                                                                              Float)] (index k
                                                                                                                                             alphas)
                                                                                                                                      (sum t$6176))
                                                                                                                 ([mul (Tuple Float
                                                                                                                              Float)] 0.5
                                                                                                                                      ([sqnorm (Vec Float)] ([mul (Tuple (Tensor 2 Float)
                                                                                                                                                                         (Vec Float))] ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                  (Vec Float))] t$6176
                                                                                                                                                                                                                                (index k
                                                                                                                                                                                                                                       ls))
                                                                                                                                                                                       ([sub (Tuple (Vec Float)
                                                                                                                                                                                                    (Vec Float))] (index i
                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                  (index k
                                                                                                                                                                                                                         means)))))))))))))
     (t$6189 ([to_float Integer] N))
     (t$6190 ([logsumexp (Vec Float)] alphas))
     (t$6191 ([mul (Tuple Float Float)] t$6189 t$6190))
     (t$6196 ([sub (Tuple Float Float)] slse t$6191))
     (t$6202 ([add (Tuple Float Float)] CONSTANT t$6196))
     (t$6207 (get$2$2 _t1))
     (ksc$argVar_27 (constVec N
                              (get$1$2 ([rev [sub (Tuple Float Float)]] (tuple slse t$6191)
                                                                        (get$2$2 ([rev [add (Tuple Float
                                                                                                   Float)]] (tuple CONSTANT
                                                                                                                   t$6196)
                                                                                                            (get$1$2 ([rev [add (Tuple Float
                                                                                                                                       Float)]] (tuple t$6202
                                                                                                                                                       (sumbuild K
                                                                                                                                                                 (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                                                                                                                                                      Integer)
                                                                                                                                                                                                               (Vec Float)
                                                                                                                                                                                                               (Vec Float))] wishart
                                                                                                                                                                                                                             (index k
                                                                                                                                                                                                                                    qs)
                                                                                                                                                                                                                             (index k
                                                                                                                                                                                                                                    ls)))))
                                                                                                                                                t$6207))))))))
     (ksc$argVar_35 (constVec K
                              (get$2$2 ([rev [add (Tuple Float Float)]] (tuple t$6202
                                                                               (sumbuild K
                                                                                         (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                                                                              Integer)
                                                                                                                                       (Vec Float)
                                                                                                                                       (Vec Float))] wishart
                                                                                                                                                     (index k
                                                                                                                                                            qs)
                                                                                                                                                     (index k
                                                                                                                                                            ls)))))
                                                                        t$6207))))
     (rlse (build N (lam (i : Integer) ([rev [logsumexp (Vec Float)]] (build K
                                                                                                        (lam (k : Integer) (let
                                                                                                                            ((t$6227 (index k
                                                                                                                                            qs)))
                                                                                                                            ([sub (Tuple Float
                                                                                                                                         Float)] ([add (Tuple Float
                                                                                                                                                              Float)] (index k
                                                                                                                                                                             alphas)
                                                                                                                                                                      (sum t$6227))
                                                                                                                                                 ([mul (Tuple Float
                                                                                                                                                              Float)] 0.5
                                                                                                                                                                      ([sqnorm (Vec Float)] ([mul (Tuple (Tensor 2 Float)
                                                                                                                                                                                                         (Vec Float))] ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                                                  (Vec Float))] t$6227
                                                                                                                                                                                                                                                                (index k
                                                                                                                                                                                                                                                                       ls))
                                                                                                                                                                                                                       ([sub (Tuple (Vec Float)
                                                                                                                                                                                                                                    (Vec Float))] (index i
                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                  (index k
                                                                                                                                                                                                                                                         means)))))))))
                                                                                                 (index i
                                                                                                        ksc$argVar_27)))))
     (sum1 (buildFromSparseTupled
                              (tuple (constVec (size x) (constVec (size (index 0 x)) (tuple)))
                                     (constVec (size alphas) (tuple))
                                     (constVec (size means) (constVec (size (index 0 means)) (tuple)))
                                     (constVec (size qs) (constVec (size (index 0 qs)) (tuple)))
                                     (constVec (size ls) (constVec (size (index 0 ls)) (tuple))))
                              (tuple N K)
                              (lam (ik : (Tuple Integer Integer))
                                                                             (let
                                                                                (((i k) ik)
                                                                                 (ksc$argVar_47 (index i rlse))
                                                                                 (t$6241 (index k
                                                                                                qs))
                                                                                 (t$6242 (index k
                                                                                                ls))
                                                                                 (Q ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                               (Vec Float))] t$6241
                                                                                                                             t$6242))
                                                                                 (t$6243 (index i
                                                                                                x))
                                                                                 (t$6244 (index k
                                                                                                means))
                                                                                 (t$6245 ([sub (Tuple (Vec Float)
                                                                                                      (Vec Float))] t$6243
                                                                                                                    t$6244))
                                                                                 (mahal_vec ([mul (Tuple (Tensor 2 Float)
                                                                                                         (Vec Float))] Q
                                                                                                                       t$6245))
                                                                                 (t$6254 ([sqnorm (Vec Float)] mahal_vec))
                                                                                 (t$6256 (index k
                                                                                                alphas))
                                                                                 (t$6258 (sum t$6241))
                                                                                 (t$6264 ([rev [sub (Tuple Float
                                                                                                           Float)]] (tuple ([add (Tuple Float
                                                                                                                                        Float)] t$6256
                                                                                                                                                t$6258)
                                                                                                                           ([mul (Tuple Float
                                                                                                                                        Float)] 0.5
                                                                                                                                                t$6254))
                                                                                                                    (index k
                                                                                                                           ksc$argVar_47)))
                                                                                 (t$6269 ([rev [mul (Tuple (Tensor 2 Float)
                                                                                                           (Vec Float))]] (tuple Q
                                                                                                                                 t$6245)
                                                                                                                          ([rev [sqnorm (Vec Float)]] mahal_vec
                                                                                                                                                      (get$2$2 ([rev [mul (Tuple Float
                                                                                                                                                                                 Float)]] (tuple 0.5
                                                                                                                                                                                                 t$6254)
                                                                                                                                                                                          (get$2$2 t$6264))))))
                                                                                 (t$6271 ([rev [sub (Tuple (Vec Float)
                                                                                                           (Vec Float))]] (tuple t$6243
                                                                                                                                 t$6244)
                                                                                                                          (get$2$2 t$6269)))
                                                                                 (t$6289 ([rev [add (Tuple Float
                                                                                                           Float)]] (tuple t$6256
                                                                                                                           t$6258)
                                                                                                                    (get$1$2 t$6264)))
                                                                                 (t$6320 (size qs))
                                                                                 (t$6366 ([rev [gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                         (Vec Float))]] (tuple t$6241
                                                                                                                                               t$6242)
                                                                                                                                        (get$1$2 t$6269))))
                                                                                (tuple (tuple
                                                                                                 i
                                                                                                 (get$1$2 t$6271))
                                                                                       (tuple
                                                                                                 k
                                                                                                 (get$1$2 t$6289))
                                                                                       (tuple
                                                                                                 k
                                                                                                 (get$2$2 t$6271))
                                                                                       (tuple
                                                                                                 k
                                                                                                 (ts_add (constVec (size t$6241)
                                                                                                                   (get$2$2 t$6289))
                                                                                                         (get$1$2 t$6366)))
                                                                                       (tuple
                                                                                                 k
                                                                                                 (get$2$2 t$6366))))))))
              (tuple (get$1$5 sum1)
                     (ts_add (get$2$5 sum1)
                           ([rev [logsumexp (Vec Float)]] alphas
                                                          (get$2$2 ([rev [mul (Tuple Float
                                                                                     Float)]] (tuple t$6189
                                                                                                     t$6190)
                                                                                              (get$2$2 ([rev [sub (Tuple Float
                                                                                                                         Float)]] (tuple slse
                                                                                                                                         t$6191)
                                                                                                                                  (get$2$2 ([rev [add (Tuple Float
                                                                                                                                                             Float)]] (tuple CONSTANT
                                                                                                                                                                             t$6196)
                                                                                                                                                                      (get$1$2 ([rev [add (Tuple Float
                                                                                                                                                                                                 Float)]] (tuple t$6202
                                                                                                                                                                                                                 (sumbuild K
                                                                                                                                                                                                                           (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                                                                                                                                                                                                                Integer)
                                                                                                                                                                                                                                                                         (Vec Float)
                                                                                                                                                                                                                                                                         (Vec Float))] wishart
                                                                                                                                                                                                                                                                                       (index k
                                                                                                                                                                                                                                                                                              qs)
                                                                                                                                                                                                                                                                                       (index k
                                                                                                                                                                                                                                                                                              ls)))))
                                                                                                                                                                                                          t$6207))))))))))
                     (get$3$5 sum1)
                     (ts_add (get$4$5 sum1)
                             (build K
                          (lam (k : Integer) (get$2$3 ([rev [log_wishart_prior (Tuple (Tuple Float
                                                                                             Integer)
                                                                                      (Vec Float)
                                                                                      (Vec Float))]] (tuple wishart
                                                                                                            (index k
                                                                                                                   qs)
                                                                                                            (index k
                                                                                                                   ls))
                                                                                                     (index k
                                                                                                            ksc$argVar_35))))))
                     (ts_add (get$5$5 sum1)
                             (build K
                          (lam (k : Integer) (get$3$3 ([rev [log_wishart_prior (Tuple (Tuple Float
                                                                                             Integer)
                                                                                      (Vec Float)
                                                                                      (Vec Float))]] (tuple wishart
                                                                                                            (index k
                                                                                                                   qs)
                                                                                                            (index k
                                                                                                                   ls))
                                                                                                     (index k
                                                                                                            ksc$argVar_35))))))
                     (sumbuild K
                             (lam (k : Integer) (get$1$3 ([rev [log_wishart_prior (Tuple (Tuple Float
                                                                                                Integer)
                                                                                         (Vec Float)
                                                                                         (Vec Float))]] (tuple wishart
                                                                                                               (index k
                                                                                                                      qs)
                                                                                                               (index k
                                                                                                                      ls))
                                                                                                        (index k
                                                                                                               ksc$argVar_35))))))))))
(def
 [shape [rev [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                               (Vec Float)
                                               (Vec (Vec Float))
                                               (Vec (Vec Float))
                                               (Vec (Vec Float))
                                               (Tuple Float Integer))]]] (Tuple (Vec (Vec (Tuple)))
                                                                                (Vec (Tuple))
                                                                                (Vec (Vec (Tuple)))
                                                                                (Vec (Vec (Tuple)))
                                                                                (Vec (Vec (Tuple)))
                                                                                (Tuple (Tuple)
                                                                                       (Tuple)))
 (_t1 : (Tuple (Tuple (Vec (Vec Float))
                      (Vec Float)
                      (Vec (Vec Float))
                      (Vec (Vec Float))
                      (Vec (Vec Float))
                      (Tuple Float Integer))
               Float))
 (let
  ((_t (get$1$2 _t1))
   (x (get$1$6 _t))
   (alphas (get$2$6 _t))
   (means (get$3$6 _t))
   (qs (get$4$6 _t))
   (ls (get$5$6 _t))
   (wishart (get$6$6 _t))
   (N (size x))
   (K (size alphas))
   (D (size (index 0 x)))
   (t$6517 ([gmm_knossos_tri Integer] D)))
  (shape (assert (eq (size (index 0 ls)) t$6517)
          (let
           ((CONSTANT ([mul (Tuple Float
                                   Float)] ([to_float Integer] ([mul (Tuple Integer Integer)] N D))
                                           ([neg Float] 0.9189385332046727)))
            (slse (sumbuild N
                            (lam (i : Integer) ([logsumexp (Vec Float)] (build K
                                                                               (lam (k : Integer) (let
                                                                                                   ((t$6522 (index k
                                                                                                                   qs)))
                                                                                                   ([sub (Tuple Float
                                                                                                                Float)] ([add (Tuple Float
                                                                                                                                     Float)] (index k
                                                                                                                                                    alphas)
                                                                                                                                             (sum t$6522))
                                                                                                                        ([mul (Tuple Float
                                                                                                                                     Float)] 0.5
                                                                                                                                             ([sqnorm (Vec Float)] ([mul (Tuple (Tensor 2 Float)
                                                                                                                                                                                (Vec Float))] ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                         (Vec Float))] t$6522
                                                                                                                                                                                                                                       (index k
                                                                                                                                                                                                                                              ls))
                                                                                                                                                                                              ([sub (Tuple (Vec Float)
                                                                                                                                                                                                           (Vec Float))] (index i
                                                                                                                                                                                                                                x)
                                                                                                                                                                                                                         (index k
                                                                                                                                                                                                                                means)))))))))))))
            (t$6535 ([to_float Integer] N))
            (t$6536 ([logsumexp (Vec Float)] alphas))
            (t$6537 ([mul (Tuple Float Float)] t$6535 t$6536))
            (t$6542 ([sub (Tuple Float Float)] slse t$6537))
            (t$6548 ([add (Tuple Float Float)] CONSTANT t$6542))
            (t$6553 (get$2$2 _t1))
            (ksc$argVar_27 (constVec N
                                     (get$1$2 ([rev [sub (Tuple Float Float)]] (tuple slse t$6537)
                                                                               (get$2$2 ([rev [add (Tuple Float
                                                                                                          Float)]] (tuple CONSTANT
                                                                                                                          t$6542)
                                                                                                                   (get$1$2 ([rev [add (Tuple Float
                                                                                                                                              Float)]] (tuple t$6548
                                                                                                                                                              (sumbuild K
                                                                                                                                                                        (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                                                                                                                                                             Integer)
                                                                                                                                                                                                                      (Vec Float)
                                                                                                                                                                                                                      (Vec Float))] wishart
                                                                                                                                                                                                                                    (index k
                                                                                                                                                                                                                                           qs)
                                                                                                                                                                                                                                    (index k
                                                                                                                                                                                                                                           ls)))))
                                                                                                                                                       t$6553))))))))
            (ksc$argVar_35 (constVec K
                                     (get$2$2 ([rev [add (Tuple Float Float)]] (tuple t$6548
                                                                                      (sumbuild K
                                                                                                (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                                                                                     Integer)
                                                                                                                                              (Vec Float)
                                                                                                                                              (Vec Float))] wishart
                                                                                                                                                            (index k
                                                                                                                                                                   qs)
                                                                                                                                                            (index k
                                                                                                                                                                   ls)))))
                                                                               t$6553)))))
           (ts_add (ts_add (sumbuild N
                                     (lam (i : Integer) (let
                                                         ((ksc$argVar_47 ([rev [logsumexp (Vec Float)]] (build K
                                                                                                               (lam (k : Integer) (let
                                                                                                                                   ((t$6573 (index k
                                                                                                                                                   qs)))
                                                                                                                                   ([sub (Tuple Float
                                                                                                                                                Float)] ([add (Tuple Float
                                                                                                                                                                     Float)] (index k
                                                                                                                                                                                    alphas)
                                                                                                                                                                             (sum t$6573))
                                                                                                                                                        ([mul (Tuple Float
                                                                                                                                                                     Float)] 0.5
                                                                                                                                                                             ([sqnorm (Vec Float)] ([mul (Tuple (Tensor 2 Float)
                                                                                                                                                                                                                (Vec Float))] ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                                                         (Vec Float))] t$6573
                                                                                                                                                                                                                                                                       (index k
                                                                                                                                                                                                                                                                              ls))
                                                                                                                                                                                                                              ([sub (Tuple (Vec Float)
                                                                                                                                                                                                                                           (Vec Float))] (index i
                                                                                                                                                                                                                                                                x)
                                                                                                                                                                                                                                                         (index k
                                                                                                                                                                                                                                                                means)))))))))
                                                                                                        (index i
                                                                                                               ksc$argVar_27))))
                                                         (sumbuild K
                                                                   (lam (k : Integer) (let
                                                                                       ((t$6587 (index k
                                                                                                       qs))
                                                                                        (t$6588 (index k
                                                                                                       ls))
                                                                                        (Q ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                      (Vec Float))] t$6587
                                                                                                                                    t$6588))
                                                                                        (t$6589 (index i
                                                                                                       x))
                                                                                        (t$6590 (index k
                                                                                                       means))
                                                                                        (t$6591 ([sub (Tuple (Vec Float)
                                                                                                             (Vec Float))] t$6589
                                                                                                                           t$6590))
                                                                                        (mahal_vec ([mul (Tuple (Tensor 2 Float)
                                                                                                                (Vec Float))] Q
                                                                                                                              t$6591))
                                                                                        (t$6600 ([sqnorm (Vec Float)] mahal_vec))
                                                                                        (t$6602 (index k
                                                                                                       alphas))
                                                                                        (t$6604 (sum t$6587))
                                                                                        (t$6610 ([rev [sub (Tuple Float
                                                                                                                  Float)]] (tuple ([add (Tuple Float
                                                                                                                                               Float)] t$6602
                                                                                                                                                       t$6604)
                                                                                                                                  ([mul (Tuple Float
                                                                                                                                               Float)] 0.5
                                                                                                                                                       t$6600))
                                                                                                                           (index k
                                                                                                                                  ksc$argVar_47)))
                                                                                        (t$6615 ([rev [mul (Tuple (Tensor 2 Float)
                                                                                                                  (Vec Float))]] (tuple Q
                                                                                                                                        t$6591)
                                                                                                                                 ([rev [sqnorm (Vec Float)]] mahal_vec
                                                                                                                                                             (get$2$2 ([rev [mul (Tuple Float
                                                                                                                                                                                        Float)]] (tuple 0.5
                                                                                                                                                                                                        t$6600)
                                                                                                                                                                                                 (get$2$2 t$6610))))))
                                                                                        (t$6617 ([rev [sub (Tuple (Vec Float)
                                                                                                                  (Vec Float))]] (tuple t$6589
                                                                                                                                        t$6590)
                                                                                                                                 (get$2$2 t$6615)))
                                                                                        (t$6635 ([rev [add (Tuple Float
                                                                                                                  Float)]] (tuple t$6602
                                                                                                                                  t$6604)
                                                                                                                           (get$1$2 t$6610)))
                                                                                        (t$6666 (size qs))
                                                                                        (t$6712 ([rev [gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                (Vec Float))]] (tuple t$6587
                                                                                                                                                      t$6588)
                                                                                                                                               (get$1$2 t$6615))))
                                                                                       (tuple (deltaVec N
                                                                                                        i
                                                                                                        (get$1$2 t$6617))
                                                                                              (deltaVec K
                                                                                                        k
                                                                                                        (get$1$2 t$6635))
                                                                                              (deltaVec (size means)
                                                                                                        k
                                                                                                        (get$2$2 t$6617))
                                                                                              (ts_add (deltaVec t$6666
                                                                                                                k
                                                                                                                (constVec (size t$6587)
                                                                                                                          (get$2$2 t$6635)))
                                                                                                      (deltaVec t$6666
                                                                                                                k
                                                                                                                (get$1$2 t$6712)))
                                                                                              (deltaVec (size ls)
                                                                                                        k
                                                                                                        (get$2$2 t$6712))
                                                                                              (tuple 0.0
                                                                                                     (tuple)))))))))
                           (tuple (constVec N (constVec D 0.0))
                                  ([rev [logsumexp (Vec Float)]] alphas
                                                                 (get$2$2 ([rev [mul (Tuple Float
                                                                                            Float)]] (tuple t$6535
                                                                                                            t$6536)
                                                                                                     (get$2$2 ([rev [sub (Tuple Float
                                                                                                                                Float)]] (tuple slse
                                                                                                                                                t$6537)
                                                                                                                                         (get$2$2 ([rev [add (Tuple Float
                                                                                                                                                                    Float)]] (tuple CONSTANT
                                                                                                                                                                                    t$6542)
                                                                                                                                                                             (get$1$2 ([rev [add (Tuple Float
                                                                                                                                                                                                        Float)]] (tuple t$6548
                                                                                                                                                                                                                        (sumbuild K
                                                                                                                                                                                                                                  (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                                                                                                                                                                                                                       Integer)
                                                                                                                                                                                                                                                                                (Vec Float)
                                                                                                                                                                                                                                                                                (Vec Float))] wishart
                                                                                                                                                                                                                                                                                              (index k
                                                                                                                                                                                                                                                                                                     qs)
                                                                                                                                                                                                                                                                                              (index k
                                                                                                                                                                                                                                                                                                     ls)))))
                                                                                                                                                                                                                 t$6553)))))))))
                                  (constVec (size means) (constVec (size (index 0 means)) 0.0))
                                  (constVec (size qs) (constVec (size (index 0 qs)) 0.0))
                                  (constVec (size ls) (constVec t$6517 0.0))
                                  (tuple 0.0 (tuple))))
                   (tuple (constVec N (constVec D 0.0))
                          (constVec K 0.0)
                          (constVec (size means) (constVec (size (index 0 means)) 0.0))
                          (build K
                                 (lam (k : Integer) (get$2$3 ([rev [log_wishart_prior (Tuple (Tuple Float
                                                                                                    Integer)
                                                                                             (Vec Float)
                                                                                             (Vec Float))]] (tuple wishart
                                                                                                                   (index k
                                                                                                                          qs)
                                                                                                                   (index k
                                                                                                                          ls))
                                                                                                            (index k
                                                                                                                   ksc$argVar_35)))))
                          (build K
                                 (lam (k : Integer) (get$3$3 ([rev [log_wishart_prior (Tuple (Tuple Float
                                                                                                    Integer)
                                                                                             (Vec Float)
                                                                                             (Vec Float))]] (tuple wishart
                                                                                                                   (index k
                                                                                                                          qs)
                                                                                                                   (index k
                                                                                                                          ls))
                                                                                                            (index k
                                                                                                                   ksc$argVar_35)))))
                          (sumbuild K
                                    (lam (k : Integer) (get$1$3 ([rev [log_wishart_prior (Tuple (Tuple Float
                                                                                                       Integer)
                                                                                                (Vec Float)
                                                                                                (Vec Float))]] (tuple wishart
                                                                                                                      (index k
                                                                                                                             qs)
                                                                                                                      (index k
                                                                                                                             ls))
                                                                                                               (index k
                                                                                                                      ksc$argVar_35))))))))))))
(def
 [CL [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                       (Vec Float)
                                       (Vec (Vec Float))
                                       (Vec (Vec Float))
                                       (Vec (Vec Float))
                                       (Tuple Float Integer))]] Float
 ((x : Vec (Vec Float))
  (alphas : Vec Float)
  (means : Vec (Vec Float))
  (qs : Vec (Vec Float))
  (ls : Vec (Vec Float))
  (wishart : (Tuple Float Integer)))
 (let
  ((N (size x))
   (K (size alphas)))
  ([add (Tuple Float Float)] ([add (Tuple Float
                                          Float)] ([mul (Tuple Float
                                                               Float)] ([to_float Integer] ([mul (Tuple Integer
                                                                                                        Integer)] N
                                                                                                                  (size (index 0
                                                                                                                               x))))
                                                                       ([neg Float] 0.9189385332046727))
                                                  ([sub (Tuple Float Float)] (sumbuild N
                                                                                       (lam (i : Integer) ([logsumexp (Vec Float)] (build K
                                                                                                                                          (lam (k : Integer) (let
                                                                                                                                                              ((t$6868 (index k
                                                                                                                                                                              qs)))
                                                                                                                                                              ([sub (Tuple Float
                                                                                                                                                                           Float)] ([add (Tuple Float
                                                                                                                                                                                                Float)] (index k
                                                                                                                                                                                                               alphas)
                                                                                                                                                                                                        (sum t$6868))
                                                                                                                                                                                   ([mul (Tuple Float
                                                                                                                                                                                                Float)] 0.5
                                                                                                                                                                                                        ([sqnorm (Vec Float)] ([mul (Tuple (Tensor 2 Float)
                                                                                                                                                                                                                                           (Vec Float))] ([gmm_knossos_makeQ (Tuple (Vec Float)
                                                                                                                                                                                                                                                                                    (Vec Float))] t$6868
                                                                                                                                                                                                                                                                                                  (index k
                                                                                                                                                                                                                                                                                                         ls))
                                                                                                                                                                                                                                                         ([sub (Tuple (Vec Float)
                                                                                                                                                                                                                                                                      (Vec Float))] (index i
                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                    (index k
                                                                                                                                                                                                                                                                                           means))))))))))))
                                                                             ([mul (Tuple Float
                                                                                          Float)] ([to_float Integer] N)
                                                                                                  ([logsumexp (Vec Float)] alphas))))
                             (sumbuild K
                                       (lam (k : Integer) ([log_wishart_prior (Tuple (Tuple Float
                                                                                            Integer)
                                                                                     (Vec Float)
                                                                                     (Vec Float))] wishart
                                                                                                   (index k
                                                                                                          qs)
                                                                                                   (index k
                                                                                                          ls)))))))
(def
 [mkfloat (Tuple Integer Float)] Float
 ((seed : Integer) (scale : Float))
 ([mul (Tuple Float Float)] ([$ranhashdoub Integer] seed) scale))
(def
 [mkvec (Tuple Integer Integer Float)] (Vec Float)
 ((seed : Integer) (n : Integer) (scale : Float))
 (build n
        (lam (j : Integer) ([mkfloat (Tuple Integer
                                            Float)] ([add (Tuple Integer Integer)] j seed) scale))))
(def
 [mkvecvec (Tuple Integer Integer Integer Float)] (Vec (Vec Float))
 ((seed : Integer) (n : Integer) (m : Integer) (scale : Float))
 (build n
        (lam (j : Integer) ([mkvec (Tuple Integer
                                          Integer
                                          Float)] ([add (Tuple Integer
                                                               Integer)] ([mul (Tuple Integer
                                                                                      Integer)] j m)
                                                                         seed)
                                                  m
                                                  scale))))
(def [not_ Bool] Bool (p : Bool) (if p false true))
(def
 [main (Tuple)] Integer
 ()
 (let
  ((x ([mkvecvec (Tuple Integer
                        Integer
                        Integer
                        Float)] ([add (Tuple Integer Integer)] 0 0) 5 4 1.0))
   (alphas ([mkvec (Tuple Integer Integer Float)] ([add (Tuple Integer
                                                               Integer)] 0 1000)
                                                  10
                                                  1.0))
   (mus ([mkvecvec (Tuple Integer
                          Integer
                          Integer
                          Float)] ([add (Tuple Integer Integer)] 0 2000) 10 4 1.0))
   (qs ([mkvecvec (Tuple Integer
                         Integer
                         Integer
                         Float)] ([add (Tuple Integer Integer)] 0 3000) 10 4 0.1))
   (t$6899 ([gmm_knossos_tri Integer] 4))
   (ls ([mkvecvec (Tuple Integer
                         Integer
                         Integer
                         Float)] ([add (Tuple Integer Integer)] 0 4000) 10 t$6899 1.0))
   (dx ([mkvecvec (Tuple Integer
                         Integer
                         Integer
                         Float)] ([add (Tuple Integer Integer)] 0 5000) 5 4 1.0e-4))
   (dalphas ([mkvec (Tuple Integer
                           Integer
                           Float)] ([add (Tuple Integer Integer)] 0 6000) 10 1.0e-4))
   (dmus ([mkvecvec (Tuple Integer
                           Integer
                           Integer
                           Float)] ([add (Tuple Integer Integer)] 0 7000) 10 4 1.0e-4))
   (dqs ([mkvecvec (Tuple Integer
                          Integer
                          Integer
                          Float)] ([add (Tuple Integer Integer)] 0 8000) 10 4 1.0e-4))
   (dls ([mkvecvec (Tuple Integer
                          Integer
                          Integer
                          Float)] ([add (Tuple Integer Integer)] 0 9000) 10 t$6899 1.0e-4))
   (t$4850 ([mkfloat (Tuple Integer Float)] ([add (Tuple Integer
                                                         Integer)] 0 10000)
                                            1.0e-4))
   (gmm_at_theta ([gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                                    (Vec Float)
                                                    (Vec (Vec Float))
                                                    (Vec (Vec Float))
                                                    (Vec (Vec Float))
                                                    (Tuple Float Integer))] x
                                                                            alphas
                                                                            mus
                                                                            qs
                                                                            ls
                                                                            (tuple 3.1 7)))
   (gmm_at_theta_plus_dtheta ([gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                                                (Vec Float)
                                                                (Vec (Vec Float))
                                                                (Vec (Vec Float))
                                                                (Vec (Vec Float))
                                                                (Tuple Float Integer))] (ts_add x
                                                                                                dx)
                                                                                        (ts_add alphas
                                                                                                dalphas)
                                                                                        (ts_add mus
                                                                                                dmus)
                                                                                        (ts_add qs
                                                                                                dqs)
                                                                                        (ts_add ls
                                                                                                dls)
                                                                                        (tuple (ts_add 3.1
                                                                                                       t$4850)
                                                                                               7)))
   (gmm_fd ([sub (Tuple Float Float)] gmm_at_theta_plus_dtheta
                                      gmm_at_theta))
   (gmm_fwd ([fwd [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                                    (Vec Float)
                                                    (Vec (Vec Float))
                                                    (Vec (Vec Float))
                                                    (Vec (Vec Float))
                                                    (Tuple Float Integer))]] (tuple x
                                                                                    alphas
                                                                                    mus
                                                                                    qs
                                                                                    ls
                                                                                    (tuple 3.1 7))
                                                                             (tuple dx
                                                                                    dalphas
                                                                                    dmus
                                                                                    dqs
                                                                                    dls
                                                                                    (tuple t$4850
                                                                                           (tuple)))))
   (grad_gmm ([rev [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                                     (Vec Float)
                                                     (Vec (Vec Float))
                                                     (Vec (Vec Float))
                                                     (Vec (Vec Float))
                                                     (Tuple Float Integer))]] (tuple x
                                                                                     alphas
                                                                                     mus
                                                                                     qs
                                                                                     ls
                                                                                     (tuple 3.1 7))
                                                                              1.0))
   (checked ($check (lam (t : (Tuple (Vec (Vec Float))
                                     (Vec Float)
                                     (Vec (Vec Float))
                                     (Vec (Vec Float))
                                     (Vec (Vec Float))
                                     (Tuple Float
                                            Integer))) ([gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                                                                          (Vec Float)
                                                                                          (Vec (Vec Float))
                                                                                          (Vec (Vec Float))
                                                                                          (Vec (Vec Float))
                                                                                          (Tuple Float
                                                                                                 Integer))] t))
                    (lam (t : (Tuple (Tuple (Vec (Vec Float))
                                            (Vec Float)
                                            (Vec (Vec Float))
                                            (Vec (Vec Float))
                                            (Vec (Vec Float))
                                            (Tuple Float Integer))
                                     Float)) ([rev [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                                                                     (Vec Float)
                                                                                     (Vec (Vec Float))
                                                                                     (Vec (Vec Float))
                                                                                     (Vec (Vec Float))
                                                                                     (Tuple Float
                                                                                            Integer))]] t))
                    (tuple x alphas mus qs ls (tuple 3.1 7))
                    (tuple x alphas mus qs ls (tuple 3.1 7))
                    (tuple dx dalphas dmus dqs dls (tuple t$4850 (tuple)))
                    1.0))
   (t$6931 ([gmm_knossos_makeQ (Tuple (Vec Float)
                                      (Vec Float))] (index 0 qs) (index 0 ls))))
  (print x
         t$6931
         "\n----\n"
         ([mul (Tuple (Tensor 2 Float) (Vec Float))] t$6931 (index 0 x))
         "gmm_at_theta = "
         gmm_at_theta
         "\n----\n"
         "gmm_at_theta_CL = "
         ([CL [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                                (Vec Float)
                                                (Vec (Vec Float))
                                                (Vec (Vec Float))
                                                (Vec (Vec Float))
                                                (Tuple Float Integer))]] x
                                                                         alphas
                                                                         mus
                                                                         qs
                                                                         ls
                                                                         (tuple 3.1 7))
         "\n----\n"
         "gmm_at_theta_shape = "
         ([shape [rev [gmm_knossos_gmm_objective (Tuple (Vec (Vec Float))
                                                        (Vec Float)
                                                        (Vec (Vec Float))
                                                        (Vec (Vec Float))
                                                        (Vec (Vec Float))
                                                        (Tuple Float Integer))]]] (tuple x
                                                                                         alphas
                                                                                         mus
                                                                                         qs
                                                                                         ls
                                                                                         (tuple 3.1
                                                                                                7))
                                                                                  1.0)
         "\n----\n"
         "gmm_at_theta_plus_dtheta = "
         gmm_at_theta_plus_dtheta
         "\n----\n"
         "gmm_fwd = "
         gmm_fwd
         "\n----\n"
         "gmm_fd = "
         gmm_fd
         "\n----\n"
         "grad_gmm = "
         grad_gmm
         "\n----\n"
         "dtheta = "
         (tuple dx dalphas dmus dqs dls (tuple t$4850 (tuple)))
         "\n----\n"
         "rev_ok = "
         (tuple ([add (Tuple Float Float)] ([dot (Tuple (Vec (Vec Float))
                                                        (Vec (Vec Float)))] (get$1$6 grad_gmm) dx)
                                           ([add (Tuple Float Float)] ([dot (Tuple (Vec Float)
                                                                                   (Vec Float))] (get$2$6 grad_gmm)
                                                                                                 dalphas)
                                                                      ([add (Tuple Float
                                                                                   Float)] ([dot (Tuple (Vec (Vec Float))
                                                                                                        (Vec (Vec Float)))] (get$3$6 grad_gmm)
                                                                                                                            dmus)
                                                                                           ([add (Tuple Float
                                                                                                        Float)] ([dot (Tuple (Vec (Vec Float))
                                                                                                                             (Vec (Vec Float)))] (get$4$6 grad_gmm)
                                                                                                                                                 dqs)
                                                                                                                ([add (Tuple Float
                                                                                                                             Float)] ([dot (Tuple (Vec (Vec Float))
                                                                                                                                                  (Vec (Vec Float)))] (get$5$6 grad_gmm)
                                                                                                                                                                      dls)
                                                                                                                                     ([mul (Tuple Float
                                                                                                                                                  Float)] (get$1$2 (get$6$6 grad_gmm))
                                                                                                                                                          t$4850))))))
                " ==?== "
                gmm_fd)
         "\n----\n"
         "Checked, should be small: "
         checked
         "\n----\n"
         "TESTS FOLLOW"
         "\n----\n"
         "Golden test GMM objective\n"
         ([lt (Tuple Float Float)] ([abs Float] ([sub (Tuple Float
                                                             Float)] gmm_at_theta 76.0882))
                                   ([max (Tuple Float Float)] ([mul (Tuple Float
                                                                           Float)] ([abs Float] 76.0882)
                                                                                   1.0e-6)
                                                              1.0e-6))
         "\n----\n"
         "Reverse mode as expected\n"
         ([lt (Tuple Float Float)] checked 1.0e-4)
         "\n----\n"
         "Forward mode as expected\n"
         ([lt (Tuple Float Float)] ([abs Float] ([sub (Tuple Float
                                                             Float)] gmm_fd gmm_fwd))
                                   ([max (Tuple Float Float)] ([mul (Tuple Float
                                                                           Float)] ([abs Float] gmm_fwd)
                                                                                   1.0e-3)
                                                              1.0e-3))
         "\n----\n"
         "Not impossibly good\n"
         ([not_ Bool] (eq gmm_fd gmm_fwd)))))
