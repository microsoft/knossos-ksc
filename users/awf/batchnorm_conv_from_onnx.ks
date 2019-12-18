; An ONNX batchnorm/conv rule expressed in KS
; (bn (add (conv W X) b)) -> (add (conv sW X) + b')

(rule "bn.expand" ((X : Vec T))
    (bn X)
    (let (bn_vals (bn_compute_vals X))  ; (scale bias)
        (add (mul (get$0 bn_vals) X) (get$1 bn_vals))))           

(rule "bn.add" ((X : Vec T) (b : Vec T))
    (bn_compute_vals (add X b))
    ()

(rule "mul.conv" ((W : Vec T) (X : Vec T))
    (mul s (conv W X))
    (conv (mul s W) X))
