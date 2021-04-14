; Use --backend jax_input_last because in the jax backend we switched to
; ordering arguments as (x, weights, ...)
(edef eq Bool (Tuple Integer Integer))
(edef add (Tensor 4 Float) (Tuple (Tensor 4 Float) (Tensor 4 Float)))
(edef normalize_2d (Tensor 4 Float) (Tuple (Tuple (Vec Float) (Vec Float)) (Tensor 4 Float)))
(edef dot (Tensor 2 Float) (Tuple (Tensor 2 Float) (Tensor 2 Float)))
(edef transpose (Tensor 2 Float) (Tensor 2 Float))
(edef broadcast_add (Tensor 2 Float) (Tuple (Tensor 2 Float) (Vec Float)))
(edef conv_2d_no_bias (Tensor 4 Float) (Tuple (Tuple Integer Integer) (Tuple Integer Integer) (Tuple (Tuple Integer Integer) (Tuple Integer Integer)) (Tensor 4 Float) (Tensor 4 Float)))
(edef batch_norm_2d (Tensor 4 Float) (Tuple (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) (Tensor 4 Float)))
(edef relu (Tensor 4 Float) (Tensor 4 Float))
(edef to_float (Tensor 4 Float) (Tensor 4 Integer))
(edef max_pool_same (Tensor 4 Float) (Tuple (Tuple Integer Integer) (Tuple Integer Integer) (Tensor 4 Float)))
(edef avg_pool_valid (Tensor 4 Float) (Tuple (Tuple Integer Integer) (Tuple Integer Integer) (Tensor 4 Float)))
(edef flatten (Tensor 2 Float) (Tensor 4 Float))
(edef log_softmax (Tensor 2 Float) (Tensor 2 Float))

(def Dense (Tensor 2 Float) ((weights : (Tuple (Tensor 2 Float) (Vec Float))) (input : (Tensor 2 Float)))
  (let ((W b) weights)
    (broadcast_add (dot input (transpose W)) b) ; transpose W to keep the shape consistent with pytorch
  )
)

(def zip2 (Vec (Tuple
                 (Tuple Integer Integer)
                 (Tuple
                   (Tuple                ; conv_block_xa_weights
                     (Tuple
                                    (Tensor 4 Float)    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Tensor 4 Float)    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Tensor 4 Float)    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                     )
                     (Tensor 4 Float) ; shortcut_conv_weights
                     (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) ; shortcut_norm_weights
                   )
                   (Vec                  ; identity_block_xy_weights
                     (Tuple
                                    (Tensor 4 Float)    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Tensor 4 Float)    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Tensor 4 Float)    ; conv_3_weights
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
                                    (Tensor 4 Float)    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Tensor 4 Float)    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Tensor 4 Float)    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                         )
                         (Tensor 4 Float) ; shortcut_conv_weights
                         (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) ; shortcut_norm_weights
                       )
                       (Vec                  ; identity_block_xy_weights
                         (Tuple
                                    (Tensor 4 Float)    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Tensor 4 Float)    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Tensor 4 Float)    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                         )
                       )
                     ))))
  (build (size vec1) (lam (i : Integer) (tuple (index i vec1) (index i vec2))))
)

(def ConvBlock (Tensor 4 Float) (
                       (strides : (Tuple Integer Integer))
                       (weights : (Tuple
                                    (Tensor 4 Float)    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Tensor 4 Float)    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Tensor 4 Float)    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                  )
                       )
                       (input : (Tensor 4 Float))
                       )
  (let ((conv_1_weights
         norm_1_weights
         conv_2_weights
         norm_2_weights
         conv_3_weights
         norm_3_weights) weights)
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


(def ConvResidualBlock (Tensor 4 Float) (
                               (strides : (Tuple Integer Integer))
                               (weights : (Tuple
                                            (Tuple
                                              (Tensor 4 Float)    ; conv_1_weights
                                              (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                              (Tensor 4 Float)    ; conv_2_weights
                                              (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                              (Tensor 4 Float)    ; conv_3_weights
                                              (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                            )
                                            (Tensor 4 Float)      ; shortcut_conv_weights
                                            (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))    ; shortcut_norm_weights
                                          )
                               )
                               (input : (Tensor 4 Float))
                               )
  (let ((conv_block_weights 
         shortcut_conv_weights
         shortcut_norm_weights) 
        weights)
  (let (main (ConvBlock strides conv_block_weights input))
  (let (shortcut (batch_norm_2d shortcut_norm_weights
                      (conv_2d_no_bias (tuple 1 1) strides (tuple (tuple 0 0) (tuple 0 0)) shortcut_conv_weights
                        input
                      )
                    ))
      (relu (add main shortcut))
    ))
  )
)

(def IdentityResidualBlock (Tensor 4 Float) (
                                   (weights : (Tuple
                                                (Tensor 4 Float)    ; conv_1_weights
                                                (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                                (Tensor 4 Float)    ; conv_2_weights
                                                (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                                (Tensor 4 Float)    ; conv_3_weights
                                                (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                              )
                                   )
                                   (input : (Tensor 4 Float))
                                  )
  (let (main (ConvBlock (tuple 1 1) weights input))
    (relu(add main input))
  )
)


(def Resnet50 (Tensor 2 Float) ((weights :
                        (Tuple
                          (Tuple (Vec Float) (Vec Float)) ; mean and std for image normalization (not trainable)
                          (Tensor 4 Float)   ; conv_weights
                          (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) ; batch_norm_weights
                          (Vec
                            (Tuple
                              (Tuple                ; conv_block_xa_weights
                                (Tuple
                                  (Tensor 4 Float)    ; conv_1_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                  (Tensor 4 Float)    ; conv_2_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                  (Tensor 4 Float)    ; conv_3_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                )
                                (Tensor 4 Float)      ; shortcut_conv_weights
                                (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))    ; shortcut_norm_weights
                              )
                              (Vec                  ; identity_block_xy_weights
                                (Tuple
                                  (Tensor 4 Float)    ; conv_1_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                  (Tensor 4 Float)    ; conv_2_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                  (Tensor 4 Float)    ; conv_3_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                )
                              )
                            )
                          )
                          (Tuple (Tensor 2 Float) (Vec Float)))) ; final_dense_weights
                      (input : Tensor 4 Integer))
  ; no longer needed
  ;(let ((blocks_filters_vec (build 4 (lam (i : Integer)
  ;                            (if (eq i 0) (tuple 64 64 256)
  ;                            (if (eq i 1) (tuple 128 128 512)
  ;                            (if (eq i 2) (tuple 256 256 1024)
  ;                            (tuple 512 512 2048)))))))
  (let (blocks_strides_vec (build 4 (lam (i : Integer)
                              (if (eq i 0) (tuple 1 1) (tuple 2 2)))))
  (let ((normalization_weights   
         conv_weights            
         batch_norm_weights      
         residual_blcoks_weights 
         final_dense_weights) 
         weights)
    (log_softmax
      (Dense final_dense_weights
        (flatten
          (avg_pool_valid (tuple 7 7) (tuple 1 1) ; pool_size=(7, 7), strides=(1, 1), padding='valid'
            (fold (lam (x_filters_strides_weights :
                          (Tuple
                            (Tensor 4 Float)               ; input
                            (Tuple
                              (Tuple Integer Integer Integer)           ; filters
                              (Tuple Integer Integer)                   ; strides
                              (Tuple                                    ; weights
                                (Tuple                ; conv_block_xa_weights
                                  (Tuple
                                    (Tensor 4 Float)    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Tensor 4 Float)    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Tensor 4 Float)    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                  )
                                  (Tensor 4 Float)      ; shortcut_conv_weights
                                  (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))    ; shortcut_norm_weights
                                )
                                (Vec                  ; identity_block_xy_weights
                                  (Tuple
                                    (Tensor 4 Float)    ; conv_1_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                    (Tensor 4 Float)    ; conv_2_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                    (Tensor 4 Float)    ; conv_3_weights
                                    (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                  )
                                )
                              )
                            )
                          ))
              (let ((x filters_strides_weights) x_filters_strides_weights)
              (let ((strides weights) filters_strides_weights)
              (let ((conv_block_weights identity_blocks_weights) weights)
              (let (conv_block_out (ConvResidualBlock strides conv_block_weights x))
                      (fold (lam (x_weights :
                                    (Tuple
                                      (Tensor 4 Float)
                                      (Tuple
                                        (Tensor 4 Float)    ; conv_1_weights
                                        (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_1_weights
                                        (Tensor 4 Float)    ; conv_2_weights
                                        (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_2_weights
                                        (Tensor 4 Float)    ; conv_3_weights
                                        (Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float))  ; norm_3_weights
                                      )
                                    ))
                        (let ((x identity_block_weights) x_weights)
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
))
