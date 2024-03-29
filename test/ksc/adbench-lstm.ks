; LSTM as implemented in the ADBench suite.  It is not a standard LSTM
; but it is an exact copy of ADBench's version so that it is
; meaningful to compare their performance.
;
; See
;
;     https://github.com/awf/ADBench/blob/master/src/cpp/shared/lstm.h

(def sigmoid Float (x : Float)
     (div 1.0 (add 1.0 (exp (neg x)))))

(gdef fwd [sigmoid Float])
(gdef rev [sigmoid Float])
(gdef suffwdpass [sigmoid Float])
(gdef sufrevpass [sigmoid Float])
(gdef sufrev [sigmoid Float])

(def exp$VecR (Vec Float) ((v : Vec Float))
 (let (n (size v))
  (build n (lam (i : Integer) (exp (index i v))))))

(gdef fwd [exp$VecR (Vec Float)])
(gdef rev [exp$VecR (Vec Float)])
(gdef suffwdpass [exp$VecR (Vec Float)])
(gdef sufrevpass [exp$VecR (Vec Float)])
(gdef sufrev [exp$VecR (Vec Float)])

; The other ADBench implementations add 2 to the logsumexp. It's not
; clear why they do that but we have to do the same to match.  See
;
;     https://github.com/awf/ADBench/issues/143
(def logsumexp Float ((v : Vec Float))
    (log (add 2.0 (sum (exp$VecR v)))))

(gdef fwd [logsumexp (Vec Float)])
(gdef rev [logsumexp (Vec Float)])
(gdef suffwdpass [logsumexp (Vec Float)])
(gdef sufrevpass [logsumexp (Vec Float)])
(gdef sufrev [logsumexp (Vec Float)])

; all Vecs size h
(def lstm_model (Tuple (Vec Float) (Vec Float))
     ((wf : Vec Float) (bf : Vec Float)
      (wi : Vec Float) (bi : Vec Float)
      (wo : Vec Float) (bo : Vec Float)
      (wc : Vec Float) (bc : Vec Float)
      (hidden : Vec Float)
      (cell   : Vec Float)
      (input  : Vec Float))

     (let ((h (size wf))
           (cell_out (build h (lam (hi : Integer)
              (let ((forget  (sigmoid (add (mul (index hi input)  (index hi wf)) (index hi bf))))
                    (ingate  (sigmoid (add (mul (index hi hidden) (index hi wi)) (index hi bi))))
                    (change  (tanh    (add (mul (index hi hidden) (index hi wc)) (index hi bc)))))
                (add (mul (index hi cell) forget) (mul ingate change))))))
           (hidden_out (build h (lam (hi : Integer)
              (let ((outgate (sigmoid (add (mul (index hi input)  (index hi wo)) (index hi bo)))))
                (mul outgate (tanh (index hi cell_out))))))))
       (tuple hidden_out cell_out)))

(gdef fwd [lstm_model
      (Tuple (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float))])
(gdef rev [lstm_model
      (Tuple (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float))])
(gdef suffwdpass [lstm_model
      (Tuple (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float))])
(gdef sufrevpass [lstm_model
      (Tuple (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float))])
(gdef sufrev [lstm_model
      (Tuple (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float))])

; Return (Tuple (Vec h Float) (Vec l (Tuple (Vec h Float) (Vec h Float)))
; wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell : Vec l <tuple of (Vec h)s>
; All others size h
(def lstm_predict (Tuple (Vec Float) (Vec (Tuple (Vec Float) (Vec Float))))
     ((wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell :
           Vec (Tuple (Vec Float) (Vec Float)
                      (Vec Float) (Vec Float)
                      (Vec Float) (Vec Float)
                      (Vec Float) (Vec Float)
                      (Vec Float) (Vec Float)))

      (in_weight  : Vec Float)
      (out_weight : Vec Float)
      (out_bias   : Vec Float)

      (input : Vec Float))

     (let ((h (size in_weight))
           (l (size wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
           (output1 (build h (lam (bi : Integer) (mul (index bi input) (index bi in_weight)))))
           (final_output_i_o_v (fold (lam
               (layer_output_params
                : (Tuple (Tuple Integer (Vec Float) (Vec (Tuple (Vec Float) (Vec Float))))
                         (Tuple (Vec Float) (Vec Float)
                                (Vec Float) (Vec Float)
                                (Vec Float) (Vec Float)
                                (Vec Float) (Vec Float)
                                (Vec Float) (Vec Float))))
               (let (((i_layer_output_vec params) layer_output_params)
                     ((iteration layer_output vec_output) i_layer_output_vec)
                     ((wf bf wi bi wo bo wc bc hidden cell) params)
                     (hidden_cell (lstm_model wf bf wi bi wo bo wc bc hidden cell layer_output))
                     (layer_output_next (get$1$2 hidden_cell))
                     (vec_output_next
                          (build l (lam (li : Integer)
                              (if (eq li iteration)
                                  hidden_cell
                                (index li vec_output))))))
                 (tuple (add iteration 1) layer_output_next vec_output_next)))

                         (tuple 0
                                output1
                                (constVec l (tuple (constVec h 0.0) (constVec h 0.0))))
                         wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))

           (final_output (get$2$3 final_output_i_o_v))
           (final_output_vec (get$3$3 final_output_i_o_v))
           (output (build h (lam (bi : Integer)
                       (add (mul (index bi final_output) (index bi out_weight))
                            (index bi out_bias))))))
       (tuple output final_output_vec)))

(gdef fwd [lstm_predict
      (Tuple (Vec (Tuple (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)))
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float))])
(gdef rev [lstm_predict
      (Tuple (Vec (Tuple (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)))
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec Float))])
; SUF/BOG-AD doesn't support fold
;; (gdef suffwdpass [lstm_predict
;;       (Tuple (Vec (Tuple (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)))
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float))])
;; (gdef sufrevpass [lstm_predict
;;       (Tuple (Vec (Tuple (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)))
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float))])
;; (gdef sufrev [lstm_predict
;;       (Tuple (Vec (Tuple (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)))
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float))])

; sequence: Vec cm1 <tuple of (Vec h)s>
(def lstm_objective Float
     ((wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell :
           Vec (Tuple (Vec Float) (Vec Float)
                      (Vec Float) (Vec Float)
                      (Vec Float) (Vec Float)
                      (Vec Float) (Vec Float)
                      (Vec Float) (Vec Float)))

      (in_weight  : Vec Float)
      (out_weight : Vec Float)
      (out_bias   : Vec Float)
      (sequence : Vec (Tuple (Vec Float) (Vec Float))))

     (let ((l (size wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
           (h (size in_weight))
           (cm1 (size sequence))
           (total_hidden (fold (lam (total_data_gold
               : (Tuple (Tuple Float (Vec (Tuple (Vec Float) (Vec Float))))
                         (Tuple (Vec Float) (Vec Float))))
                   (let ((total_hidden (get$1$2 total_data_gold))
                         (total (get$1$2 total_hidden))
                         (hidden_cell (get$2$2 total_hidden))
                         (data_gold (get$2$2 total_data_gold))
                         (data (get$1$2 data_gold))
                         (ygold (get$2$2 data_gold))
                         (wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell_next
                          (build l (lam (li : Integer)
                             (tuple (get$1$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                    (get$2$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                    (get$3$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                    (get$4$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                    (get$5$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                    (get$6$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                    (get$7$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                    (get$8$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                    (get$1$2 (index li hidden_cell))
                                    (get$2$2 (index li hidden_cell))))))
                         (ypred_v (lstm_predict wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell_next
                                                in_weight out_weight out_bias
                                                data))
                         ((ypred hidden_cell_next) ypred_v)
                         (lse (logsumexp ypred))
                         (ynorm (build h (lam (hi : Integer) (sub (index hi ypred) lse))))

                         (total_increment (sumbuild h (lam (hi : Integer)
                                              (mul (index hi ygold) (index hi ynorm)))))

                         (total_next (add total total_increment)))
                     (tuple total_next hidden_cell_next))
                   )
                               (tuple 0.0 (build l (lam (li : Integer) (tuple (get$9$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))
                                                                              (get$10$10 (index li wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))))))
                        sequence))
           (total (get$1$2 total_hidden))
           (count (to_float (mul cm1 h)))
           (loss (neg (div total count))))
       loss))

(gdef fwd [lstm_objective
      (Tuple (Vec (Tuple (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)))
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec (Tuple (Vec Float) (Vec Float))))])
(gdef rev [lstm_objective
      (Tuple (Vec (Tuple (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)
                         (Vec Float) (Vec Float)))
             (Vec Float)
             (Vec Float)
             (Vec Float)
             (Vec (Tuple (Vec Float) (Vec Float))))])
; SUF/BOG-AD doesn't support fold
;; (gdef suffwdpass [lstm_objective
;;       (Tuple (Vec (Tuple (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)))
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec (Tuple (Vec Float) (Vec Float))))])
;; (gdef sufrevpass [lstm_objective
;;       (Tuple (Vec (Tuple (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)))
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec (Tuple (Vec Float) (Vec Float))))])
;; (gdef sufrev [lstm_objective
;;       (Tuple (Vec (Tuple (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)
;;                          (Vec Float) (Vec Float)))
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec Float)
;;              (Vec (Tuple (Vec Float) (Vec Float))))])

(def main Integer () 0)
