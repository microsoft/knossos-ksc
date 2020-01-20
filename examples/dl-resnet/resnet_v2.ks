(edef Normalize2D (Vec (Vec (Vec (Vec Float)))) ((Tuple (Vec Float) (Vec Float)) (Vec (Vec (Vec (Vec Float))))))
(edef MatMul (Vec (Vec Float)) ((Vec (Vec Float)) (Vec (Vec Float))))
(edef Transpose (Vec (Vec Float)) ((Vec (Vec Float))))
(edef BroadcastAdd (Vec (Vec Float)) ((Vec (Vec Float)) (Vec Float)))
(edef Conv2DNoBias (Vec (Vec (Vec (Vec Float)))) (Integer (Tuple Integer Integer) (Tuple Integer Integer) (Vec (Vec (Vec (Vec Float)))) (Vec (Vec (Vec (Vec Float))))))
(edef BatchNorm2D (Vec (Vec (Vec (Vec Float)))) ((Tuple (Vec Float) (Vec Float) (Vec Float) (Vec Float)) (Vec (Vec (Vec (Vec Float))))))
(edef Relu (Vec (Vec (Vec (Vec Float)))) ((Vec (Vec (Vec (Vec Float))))))
(edef ToFloat (Vec (Vec (Vec (Vec Float)))) ((Vec (Vec (Vec (Vec Integer))))))
(edef MaxPool (Vec (Vec (Vec (Vec Float)))) ((Tuple Integer Integer) (Tuple Integer Integer) (Vec (Vec (Vec (Vec Float))))))
(edef AvgPool (Vec (Vec (Vec (Vec Float)))) ((Tuple Integer Integer) (Tuple Integer Integer) (Vec (Vec (Vec (Vec Float))))))
(edef Flatten (Vec (Vec Float)) ((Vec (Vec (Vec (Vec Float))))))
(edef LogSoftmax (Vec (Vec Float)) ((Vec (Vec Float))))

(def Dense (Vec (Vec Float)) ((weights : (Tuple (Vec (Vec Float)) (Vec Float))) (input : (Vec (Vec Float))))
  (let ((W (get$1$2 weights))
        (b (get$2$2 weights)))
    (BroadcastAdd (MatMul input (Transpose W)) b) ; transpose W to keep the shape consistent with pytorch
  )
)
(edef Add (Vec (Vec (Vec (Vec Float)))) ((Vec (Vec (Vec (Vec Float)))) (Vec (Vec (Vec (Vec Float))))))

(def zip3 (Vec (Tuple
                 (Tuple Integer Integer Integer)
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
          ((vec1 : (Vec (Tuple Integer Integer Integer)))
           (vec2 : (Vec (Tuple Integer Integer)))
           (vec3 : (Vec
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
  (build (size vec1) (lam (i : Integer) (tuple (index i vec1) (index i vec2) (index i vec3))))
)

(def ConvBlock (Vec (Vec (Vec (Vec Float)))) ((kernel_size : Integer)
                       (filters : (Tuple Integer Integer Integer))
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
    (BatchNorm2D norm_3_weights
      (Conv2DNoBias (get$3$3 filters) (tuple 1 1) (tuple 1 1) conv_3_weights
        (Relu
          (BatchNorm2D norm_2_weights
            (Conv2DNoBias (get$2$3 filters) (tuple kernel_size kernel_size) (tuple 1 1) conv_2_weights
              (Relu
                (BatchNorm2D norm_1_weights
                  (Conv2DNoBias (get$1$3 filters) (tuple 1 1) strides conv_1_weights
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


(def ConvResidualBlock (Vec (Vec (Vec (Vec Float)))) ((kernel_size : Integer)
                               (filters : (Tuple Integer Integer Integer))
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
    (let ((main (ConvBlock kernel_size filters strides conv_block_weights input))
          (shortcut (BatchNorm2D shortcut_norm_weights
                      (Conv2DNoBias (get$3$3 filters) (tuple 1 1) strides shortcut_conv_weights
                        input
                      )
                    )))
      (Relu (Add main shortcut))
    )
  )
)

(def IdentityResidualBlock (Vec (Vec (Vec (Vec Float)))) ((kernel_size : Integer)
                                   (filters : (Tuple Integer Integer Integer))
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
  (let ((main (ConvBlock kernel_size filters (tuple 1 1) weights input)))
    (Relu(Add main input))
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
  (let ((blocks_filters_vec (build 4 (lam (i : Integer)
                              (if (eq i 0) (tuple 64 64 256)
                              (if (eq i 1) (tuple 128 128 512)
                              (if (eq i 2) (tuple 256 256 1024)
                              (tuple 512 512 2048)))))))
        (blocks_strides_vec (build 4 (lam (i : Integer)
                              (if (eq i 0) (tuple 1 1) (tuple 2 2)))))
        (normalization_weights   (get$1$5 weights))
        (conv_weights            (get$2$5 weights))
        (batch_norm_weights      (get$3$5 weights))
        (residual_blcoks_weights (get$4$5 weights))
        (final_dense_weights     (get$5$5 weights)))
    (LogSoftmax
      (Dense final_dense_weights
        (Flatten
          (AvgPool (tuple 7 7) (tuple 1 1)  ; pool_size=(7, 7), strides=(1, 1)
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
                (let ((filters (get$1$3 filters_strides_weights))
                      (strides (get$2$3 filters_strides_weights))
                      (weights (get$3$3 filters_strides_weights)))
                  (let ((conv_block_weights (get$1$2 weights))
                        (identity_blocks_weights (get$2$2 weights)))
                    (let ((conv_block_out (ConvResidualBlock 3 filters strides conv_block_weights x)))
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
                          (IdentityResidualBlock 3 filters identity_block_weights x)
                        )) ; end of lam (x_weights)
                        conv_block_out
                        identity_blocks_weights)
                    )
                  )
                )
              )) ; end of lam (x_filters_strides_weights)
              (MaxPool (tuple 3 3) (tuple 2 2)                                      ; pool_size=(3, 3), strides=(2, 2)
                (Relu
                  (BatchNorm2D batch_norm_weights
                    (Conv2DNoBias 64 (tuple 7 7) (tuple 2 2) conv_weights            ; d_hidden=64, kernel_size=(7, 7), strides=(2, 2), padding="SAME"
                      (Normalize2D normalization_weights input)
                    )
                  )
                )
              )
              (zip3 blocks_filters_vec blocks_strides_vec residual_blcoks_weights)
            )
          )
        )
      )
    )
  )
)

