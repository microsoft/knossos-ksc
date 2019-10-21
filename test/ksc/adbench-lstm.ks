; LSTM as implemented in the ADBench suite.  It is not a standard LSTM
; but it is an exact copy of ADBench's version so that it is
; meaningful to compare their performance.
;
; See
;
;     https://github.com/awf/ADBench/blob/master/src/cpp/shared/lstm.h

(def sigmoid Float (x : Float)
     (div 1.0 (add 1.0 (exp (neg x)))))

(def exp$VecR (Vec n Float) ((v : Vec n Float))
  (build n (lam (i : Integer) (exp (index i v)))))

; The other ADBench implementations add 2 to the logsumexp. It's not
; clear why they do that but we have to do the same to match.  See
;
;     https://github.com/awf/ADBench/issues/143
(def logsumexp Float ((v : Vec n Float))
    (log (add 2.0 (sum (exp$VecR v)))))

(def fwd$tanh Float ((x : Float) (dx : Float))
     (let ((tanh_x (tanh x))
           (tanh_x_2 (mul tanh_x tanh_x)))
       (mul tanh_x_2 dx)))

(def rev$tanh Float ((x : Float) (d_dr : Float))
     (let ((tanh_x (tanh x))
           (tanh_x_2 (mul tanh_x tanh_x)))
       (mul tanh_x_2 d_dr)))

(edef D$tanh (LM Float Float) (Float))
(edef Dt$tanh (Tuple Float (LM Float Float)) (Float))
(edef tanh Float (Float))

(def lstm_model (Tuple (Vec h Float) (Vec h Float))
     ((wf : Vec h Float) (bf : Vec h Float)
      (wi : Vec h Float) (bi : Vec h Float)
      (wo : Vec h Float) (bo : Vec h Float)
      (wc : Vec h Float) (bc : Vec h Float)
      (hidden : Vec h Float)
      (cell   : Vec h Float)
      (input  : Vec h Float))

     (let ((cell_out (build h (lam (hi : Integer)
              (let ((forget  (sigmoid (add (mul (index hi input)  (index hi wf)) (index hi bf))))
                    (ingate  (sigmoid (add (mul (index hi hidden) (index hi wi)) (index hi bi))))
                    (change  (tanh    (add (mul (index hi hidden) (index hi wc)) (index hi bc)))))
                (add (mul (index hi cell) forget) (mul ingate change))))))
           (hidden_out (build h (lam (hi : Integer)
              (let ((outgate (sigmoid (add (mul (index hi input)  (index hi wo)) (index hi bo)))))
                (mul outgate (tanh (index hi cell_out))))))))
       (tuple hidden_out cell_out)))

(def lstm_predict (Tuple (Vec h Float) (Vec l (Tuple (Vec h Float) (Vec h Float))))
     ((wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell :
           Vec l (Tuple (Vec h Float) (Vec h Float)
                        (Vec h Float) (Vec h Float)
                        (Vec h Float) (Vec h Float)
                        (Vec h Float) (Vec h Float)
                        (Vec h Float) (Vec h Float)))

      (in_weight  : Vec h Float)
      (out_weight : Vec h Float)
      (out_bias   : Vec h Float)

      (input : Vec h Float))

     (let ((output1 (build h (lam (bi : Integer) (mul (index bi input) (index bi in_weight)))))
           (final_output_i_o_v (fold (lam
               (layer_output_params
                : (Tuple (Tuple Integer (Vec h Float) (Vec l (Tuple (Vec h Float) (Vec h Float))))
                         (Tuple (Vec h Float) (Vec h Float)
                                (Vec h Float) (Vec h Float)
                                (Vec h Float) (Vec h Float)
                                (Vec h Float) (Vec h Float)
                                (Vec h Float) (Vec h Float))))
               (let ((i_layer_output_vec (get$1$2 layer_output_params))
                     (iteration (get$1$3 i_layer_output_vec))
                     (layer_output (get$2$3 i_layer_output_vec))
                     (vec_output (get$3$3 i_layer_output_vec))
                     (params (get$2$2 layer_output_params))
                     (wf (get$1$10 params))
                     (bf (get$2$10 params))
                     (wi (get$3$10 params))
                     (bi (get$4$10 params))
                     (wo (get$5$10 params))
                     (bo (get$6$10 params))
                     (wc (get$7$10 params))
                     (bc (get$8$10 params))
                     (hidden (get$9$10 params))
                     (cell (get$10$10 params))
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
                                (build l (lam (li : Integer)
                                    (tuple (constVec h 0.0) (constVec h 0.0)))))
                         wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell))

           (final_output (get$2$3 final_output_i_o_v))
           (final_output_vec (get$3$3 final_output_i_o_v))
           (output (build h (lam (bi : Integer)
                       (add (mul (index bi final_output) (index bi out_weight))
                            (index bi out_bias))))))
       (tuple output final_output_vec)))

(def lstm_objective Float
     ((wf_bf_wi_bi_wo_bo_wc_bc_hidden_cell :
           Vec l (Tuple (Vec h Float) (Vec h Float)
                        (Vec h Float) (Vec h Float)
                        (Vec h Float) (Vec h Float)
                        (Vec h Float) (Vec h Float)
                        (Vec h Float) (Vec h Float)))

      (in_weight  : Vec h Float)
      (out_weight : Vec h Float)
      (out_bias   : Vec h Float)
      (sequence : Vec cm1 (Tuple (Vec h Float) (Vec h Float))))

     (let ((total_hidden (fold (lam (total_data_gold
               : (Tuple (Tuple Float (Vec l (Tuple (Vec h Float) (Vec h Float))))
                         (Tuple (Vec h Float) (Vec h Float))))
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
                         (ypred (get$1$2 ypred_v))
                         (hidden_cell_next (get$2$2 ypred_v))
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
