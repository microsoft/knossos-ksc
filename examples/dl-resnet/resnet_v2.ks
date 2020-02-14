; Use --backend jax_input_last because in the jax backend we switched to
; ordering arguments as (x, weights, ...)
(edef normalize_2d (Vec (Vec (Vec (Vec Float)))) ((Tuple (Vec Float) (Vec Float)) (Vec (Vec (Vec (Vec Float))))))
(edef dot (Vec (Vec Float)) ((Vec (Vec Float)) (Vec (Vec Float))))
(edef transpose (Vec (Vec Float)) ((Vec (Vec Float))))
(edef broadcast_add (Vec (Vec Float)) ((Vec (Vec Float)) (Vec Float)))
(edef conv_2d_no_bias (Vec (Vec (Vec (Vec Float)))) ((Tuple Integer Integer) (Tuple Integer Integer) (Tuple (Tuple Integer Integer) (Tuple Integer Integer)) (Vec (Vec (Vec (Vec Float)))) (Vec (Vec (Vec (Vec Float))))))
(edef batch_norm_2d (Vec (Vec (Vec (Vec Float)))) ((Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) (Vec (Vec (Vec (Vec Float))))))
(edef relu (Vec (Vec (Vec (Vec Float)))) ((Vec (Vec (Vec (Vec Float))))))
(edef to_float (Vec (Vec (Vec (Vec Float)))) ((Vec (Vec (Vec (Vec Integer))))))
(edef max_pool_same (Vec (Vec (Vec (Vec Float)))) ((Tuple Integer Integer) (Tuple Integer Integer) (Vec (Vec (Vec (Vec Float))))))
(edef avg_pool_valid (Vec (Vec (Vec (Vec Float)))) ((Tuple Integer Integer) (Tuple Integer Integer) (Vec (Vec (Vec (Vec Float))))))
(edef flatten (Vec (Vec Float)) ((Vec (Vec (Vec (Vec Float))))))
(edef log_softmax (Vec (Vec Float)) ((Vec (Vec Float))))

(def Dense (Vec (Vec Float)) ((weights : (Tuple (Vec (Vec Float)) (Vec Float))) (input : (Vec (Vec Float))))
  (let ((W (get$1$2 weights))
        (b (get$2$2 weights)))
    (broadcast_add (dot input (transpose W)) b) ; transpose W to keep the shape consistent with pytorch
  )
)

(def zip2 (Vec (Tuple
                 (Tuple Integer Integer)
                 (Tuple
                   (Tuple                ; conv_block_xa_weights
                     (Tuple
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                     )
                     (Vec (Vec (Vec (Vec Float)))) ; shortcut_conv_weights
                     (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) ; shortcut_norm_weights
                   )
                   (Vec                  ; identity_block_xy_weights
                     (Tuple
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                     )
                   )
                 )
               ))
          ((vec1 : (Vec (Tuple Integer Integer)))
           (vec2 : (Vec
                     (Tuple
                       (Tuple                ; conv_block_xa_weights
                         (Tuple
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                         )
                         (Vec (Vec (Vec (Vec Float)))) ; shortcut_conv_weights
                         (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) ; shortcut_norm_weights
                       )
                       (Vec                  ; identity_block_xy_weights
                         (Tuple
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                         )
                       )
                     ))))
  (build (size vec1) (lam (i : Integer) (tuple (index i vec1) (index i vec2))))
)

(def ConvBlock (Vec (Vec (Vec (Vec Float)))) (
                       (strides : (Tuple Integer Integer))
                       (weights : (Tuple
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                  )
                       )
                       (input : (Vec (Vec (Vec (Vec Float)))))
                       )
  (let ((conv_1_weights (get$1$6 weights))
        (norm_1_weights (get$2$6 weights))
        (conv_2_weights (get$3$6 weights))
        (norm_2_weights (get$4$6 weights))
        (conv_3_weights (get$5$6 weights))
        (norm_3_weights (get$6$6 weights)))
    (batch_norm_2d norm_3_weights
      (conv_2d_no_bias (tuple 1 1) (tuple 1 1) (tuple (tuple 0 0) (tuple 0 0)) conv_3_weights
        (relu
          (batch_norm_2d norm_2_weights
            (conv_2d_no_bias (tuple 3 3)  (tuple 1 1) (tuple (tuple 1 1) (tuple 1 1)) conv_2_weights
              (relu
                (batch_norm_2d norm_1_weights
                  (conv_2d_no_bias (tuple 1 1) strides (tuple (tuple 0 0) (tuple 0 0)) conv_1_weights
                    input
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


(def ConvResidualBlock (Vec (Vec (Vec (Vec Float)))) (
                               (strides : (Tuple Integer Integer))
                               (weights : (Tuple
                                            (Tuple
                                              (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                              (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                              (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                              (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                              (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                              (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                            )
                                            (Vec (Vec (Vec (Vec Float))))      ; shortcut_conv_weights
                                            (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))    ; shortcut_norm_weights
                                          )
                               )
                               (input : (Vec (Vec (Vec (Vec Float)))))
                               )
  (let ((conv_block_weights (get$1$3 weights))
        (shortcut_conv_weights (get$2$3 weights))
        (shortcut_norm_weights (get$3$3 weights)))
    (let ((main (ConvBlock strides conv_block_weights input))
          (shortcut (batch_norm_2d shortcut_norm_weights
                      (conv_2d_no_bias (tuple 1 1) strides (tuple (tuple 0 0) (tuple 0 0)) shortcut_conv_weights
                        input
                      )
                    )))
      (relu (add main shortcut))
    )
  )
)

(def IdentityResidualBlock (Vec (Vec (Vec (Vec Float)))) (
                                   (weights : (Tuple
                                                (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                                (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                                (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                                (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                                (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                                (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                              )
                                   )
                                   (input : (Vec (Vec (Vec (Vec Float)))))
                                  )
  (let ((main (ConvBlock (tuple 1 1) weights input)))
    (relu(add main input))
  )
)


(def Resnet50 (Vec (Vec Float)) ((weights :
                        (Tuple
                          (Tuple (Vec Float) (Vec Float)) ; mean and std for image normalization (not trainable)
                          (Vec (Vec (Vec (Vec Float))))   ; conv_weights
                          (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) ; batch_norm_weights
                          (Vec
                            (Tuple
                              (Tuple                ; conv_block_xa_weights
                                (Tuple
                                  (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                  (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                  (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                )
                                (Vec (Vec (Vec (Vec Float))))      ; shortcut_conv_weights
                                (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))    ; shortcut_norm_weights
                              )
                              (Vec                  ; identity_block_xy_weights
                                (Tuple
                                  (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                  (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                  (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                )
                              )
                            )
                          )
                          (Tuple (Vec (Vec Float)) (Vec Float)))) ; final_dense_weights
                      (input : (Vec (Vec (Vec (Vec Integer))))))
  ; no longer needed
  ;(let ((blocks_filters_vec (build 4 (lam (i : Integer)
  ;                            (if (eq i 0) (tuple 64 64 256)
  ;                            (if (eq i 1) (tuple 128 128 512)
  ;                            (if (eq i 2) (tuple 256 256 1024)
  ;                            (tuple 512 512 2048)))))))
  (let ((blocks_strides_vec (build 4 (lam (i : Integer)
                              (if (eq i 0) (tuple 1 1) (tuple 2 2)))))
        (normalization_weights   (get$1$5 weights))
        (conv_weights            (get$2$5 weights))
        (batch_norm_weights      (get$3$5 weights))
        (residual_blcoks_weights (get$4$5 weights))
        (final_dense_weights     (get$5$5 weights)))
    (log_softmax
      (Dense final_dense_weights
        (flatten
          (avg_pool_valid (tuple 7 7) (tuple 1 1) ; pool_size=(7, 7), strides=(1, 1), padding='valid'
            (fold (lam (x_filters_strides_weights :
                          (Tuple
                            (Vec (Vec (Vec (Vec Float))))               ; input
                            (Tuple
                              (Tuple Integer Integer Integer)           ; filters
                              (Tuple Integer Integer)                   ; strides
                              (Tuple                                    ; weights
                                (Tuple                ; conv_block_xa_weights
                                  (Tuple
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                  )
                                  (Vec (Vec (Vec (Vec Float))))      ; shortcut_conv_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))    ; shortcut_norm_weights
                                )
                                (Vec                  ; identity_block_xy_weights
                                  (Tuple
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                  )
                                )
                              )
                            )
                          ))
              (let ((x (get$1$2 x_filters_strides_weights))
                    (filters_strides_weights (get$2$2 x_filters_strides_weights)))
                (let ((strides (get$1$2 filters_strides_weights))
                      (weights (get$2$2 filters_strides_weights)))
                  (let ((conv_block_weights (get$1$2 weights))
                        (identity_blocks_weights (get$2$2 weights)))
                    (let ((conv_block_out (ConvResidualBlock strides conv_block_weights x)))
                      (fold (lam (x_weights :
                                    (Tuple
                                      (Vec (Vec (Vec (Vec Float))))
                                      (Tuple
                                        (Vec (Vec (Vec (Vec Float))))    ; conv_1_weights
                                        (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                        (Vec (Vec (Vec (Vec Float))))    ; conv_2_weights
                                        (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                        (Vec (Vec (Vec (Vec Float))))    ; conv_3_weights
                                        (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                      )
                                    ))
                        (let ((x (get$1$2 x_weights))
                              (identity_block_weights (get$2$2 x_weights)))
                          (IdentityResidualBlock identity_block_weights x)
                        )) ; end of lam (x_weights)
                        conv_block_out
                        identity_blocks_weights)
                    )
                  )
                )
              )) ; end of lam (x_filters_strides_weights)
              (max_pool_same (tuple 3 3) (tuple 2 2)  ; pool_size=(3, 3), strides=(2, 2), padding='same'
                (relu
                  (batch_norm_2d batch_norm_weights
                    (conv_2d_no_bias (tuple 7 7) (tuple 2 2) (tuple (tuple 2 3) (tuple 2 3)) conv_weights            ; d_hidden=64, kernel_size=(7, 7), strides=(2, 2), padding="SAME"
                      (normalize_2d normalization_weights input)
                    )
                  )
                )
              )
              (zip2 blocks_strides_vec residual_blcoks_weights)
            )
          )
        )
      )
    )
  )
)

