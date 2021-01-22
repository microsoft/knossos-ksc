;; Loaders first
(edef load-from-onnx-float32 (Tensor 1 Float) (Integer String))
(edef load-from-onnx-float32 (Tensor 2 Float) (Integer Integer String))
(edef load-from-onnx-float32 (Tensor 3 Float) (Integer Integer Integer String))
(edef load-from-onnx-float32 (Tensor 4 Float) (Integer Integer Integer Integer String))
(edef load-from-onnx-int64 (Tensor 1 Integer) (Integer String))
(edef load-from-onnx-int64 (Tensor 2 Integer) (Integer Integer String))
(edef load-from-onnx-int64 (Tensor 3 Integer) (Integer Integer Integer String))
(edef load-from-onnx-int64 (Tensor 4 Integer) (Integer Integer Integer Integer String))

;; Constant
;; https://github.com/onnx/onnx/blob/master/docs/Operators.md#constant
(def Constant Float (v : Float)  v)
(def Constant Integer (v : Integer)  v)
(def Constant (Tensor 1 Float) (v : Tensor 1 Float) v)
(def Constant (Tensor 1 Integer) (v : Tensor 1 Integer) v)


(def Identity Float (a : Float) a)


;; Grads
(edef TanhGrad (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float)))

;; Broadcast versions of some builtins
(edef Sub (Tensor -1 Integer) (Integer (Tensor -1 Integer)))

(edef Sub (Tensor -1 Float) (Float (Tensor -1 Float)))
(edef Sub Float (Float Float))

(edef Mul Float (Float Float))
(edef Mul (Tensor -1 Float) (Float (Tensor -1 Float)))
(edef Mul (Tensor -1 Float) ((Tensor -1 Float) Float))
; (edef Mul (Tensor -1 Float) ((Tensor -1 Integer) (Tensor -1 Float)))

(edef Div Float (Float Float))
(edef Div (Tensor -1 Float) (Float (Tensor -1 Float)))
(edef Div (Tensor -1 Float) ((Tensor -1 Float) Float))

(edef Add Float (Float Float))
(edef Add (Tensor -1 Float) (Float (Tensor -1 Float)))
(edef Add (Tensor -1 Float) ((Tensor -1 Float) Float))

(edef Min (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float)))
(edef Min (Tensor -1 Float) (Float (Tensor -1 Float)))

(edef Equal (Tensor -1 Bool) (Float (Tensor -1 Float)))
(edef Equal (Tensor -1 Bool) ((Tensor -1 Float) Float))
(edef Equal (Tensor -1 Bool) (Integer (Tensor -1 Integer)))
(edef Equal (Tensor -1 Bool) ((Tensor -1 Integer) Integer))

;; Single-float overloads (i.e. a single float is passed as a rank-0 tensor)
(edef Gather (Tensor -1 Float) ((Tensor -1 Float) Integer Integer))
(edef com.microsoft.SoftmaxCrossEntropyLossGrad (Tensor -1 Float) (#|Vec|#Float (Tensor -1 Float) (Tensor -1 Integer) Integer String))
(edef com.microsoft.MixedPrecisionScale (Tensor -1 Float) (#|Vec|#Float (Tensor -1 Float) Integer Integer))
(edef com.microsoft.LayerNormalizationGrad (Tuple (Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float)) 
                          ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) Float Integer))
(edef com.microsoft.GatherGrad (Tensor -1 Float) ((Tensor -1 Integer) #|Vec|#Integer (Tensor -1 Float) Integer))

;; Variadics
(edef Sum (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float)))
(edef Sum (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float)))
(edef Sum (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float)))



;; Doing Dropout # line "/home/awf/dev/onnxruntime/cmake/external/onnx/onnx/defs/nn/defs.cc" 1960
; since_version 13
;; 
;; Dropout takes an input floating-point tensor, an optional input ratio (floating-point scalar) and an optional input training_mode (boolean scalar). It produces two tensor outputs,
;; output (floating-point tensor) and mask (optional `Tensor<bool>`). If `training_mode` is true then the output Y will be a random dropout;
;; Note that this Dropout scales the masked input data by the following equation, so to convert the trained model into inference mode,
;; the user can simply not pass `training_mode` input or set it to false.
;; ```
;; output = scale * data * mask,
;; ```
;; where
;; ```
;; scale = 1. / (1. - ratio).
;; ```
;; This operator has **optional** inputs/outputs. See [the doc](IR.md) for more details about the representation of optional arguments. An empty string may be used in the place of an actual argument's name to indicate a missing argument. Trailing optional arguments (those not followed by an argument that is present) may also be simply omitted.
;; Type constraints:
;; T | ['tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(bfloat16)'] | Constrain input and output types to float tensors.
;; T1 | ['tensor(float16)', 'tensor(float)', 'tensor(double)'] | Constrain input 'ratio' types to float tensors.
;; T2 | ['tensor(bool)'] | Constrain output 'mask' types to boolean tensors.
; #out=0 ARGS: Arg<data,(Tensor -1 Float),,single> Arg<ratio,(Tensor -1 Float),,optional> Arg<training_mode,(Tensor -1 Bool),,optional> Attr<Optional,seed,**MISSING**> 
(edef take1$Dropout (Tensor -1 Float) ((Tensor -1 Float) Float (Tensor -1 Bool) Integer))
(edef take1$Dropout (Tensor -1 Float) ((Tensor -1 Float) Float Integer))
(edef take1$Dropout (Tensor -1 Float) ((Tensor -1 Float) Integer))
;; NOTE: multiple outputs as tuple
; #out=1 ARGS: Arg<data,(Tensor -1 Float),,single> Arg<ratio,(Tensor -1 Float),,optional> Arg<training_mode,(Tensor -1 Bool),,optional> Attr<Optional,seed,**MISSING**> 
(edef Dropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) Float (Tensor -1 Bool) Integer))
(edef Dropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) Float Integer))
(edef Dropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) Integer))
; shape$Dropout

;; Doing TrainableDropout # line "/home/awf/dev/onnxruntime/orttraining/orttraining/core/graph/training_op_defs.cc" 1379
; since_version 9
;; TrainableDropout
;; Type constraints:
;; T | ['tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(bfloat16)'] | Constrain input and output types to float tensors.
;; T1 | ['tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(bfloat16)'] | Constrain input 'ratio' types to float tensors.
;; T2 | ['tensor(bool)'] | Constrain output 'mask' types to boolean tensors.
; #out=0 ARGS: Arg<data,(Tensor -1 Float),,single> Arg<ratio,(Tensor -1 Float),,optional> Attr<Optional,seed,**MISSING**> 
(edef take1$TrainableDropout (Tensor -1 Float) ((Tensor -1 Float) Float Integer))
(edef take1$TrainableDropout (Tensor -1 Float) ((Tensor -1 Float) Integer))
;; NOTE: multiple outputs as tuple
; #out=1 ARGS: Arg<data,(Tensor -1 Float),,single> Arg<ratio,(Tensor -1 Float),,optional> Attr<Optional,seed,**MISSING**> 
(edef TrainableDropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) Float Integer))
(edef TrainableDropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) Integer))
; shape$TrainableDropout



;; Doing com.microsoft.DropoutGrad # line "/home/awf/dev/onnxruntime/orttraining/orttraining/core/graph/training_op_defs.cc" 1450
; since_version 1
;; DropoutGrad
;; Type constraints:
;; T | ['tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(bfloat16)'] | Constrain input and output types to float tensors.
;; T1 | ['tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(bfloat16)'] | Constrain input 'ratio' types to float tensors.
;; T2 | ['tensor(bool)'] | Constrain 'mask' and 'training_mode' types to boolean tensors.
; #out=0 ARGS: Arg<dy,(Tensor -1 Float),,single> Arg<mask,(Tensor -1 Bool),,single> Arg<ratio,(Tensor -1 Float),,optional> Arg<training_mode,(Tensor -1 Bool),,optional> 
(edef com.microsoft.DropoutGrad (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Bool) Float Bool))  ; TODO: why the extra integer?
(edef com.microsoft.DropoutGrad (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Bool) Float))
(edef com.microsoft.DropoutGrad (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Bool)))
; shape$com.microsoft.DropoutGrad
(edef com.microsoft.DropoutGrad (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Bool) Float Bool Integer))  
; TODO: why the extra integer?  needed in bert-tiny-bw 'com.microsoft.DropoutGrad(%445_grad, %446, dropout_ratio_27, dropout_training_mode_28, 0)'


;; Doing com.microsoft.TrainableDropoutGrad # line "/home/awf/dev/onnxruntime/orttraining/orttraining/core/graph/training_op_defs.cc" 1418
; since_version 1
;; TrainableDropoutGrad
;; Type constraints:
;; T | ['tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(bfloat16)'] | Constrain input and output types to float tensors.
;; T1 | ['tensor(float)'] | Constrain input 'ratio' types to float tensors.
;; T2 | ['tensor(bool)'] | Constrain 'mask' types to boolean tensors.
; #out=0 ARGS: Arg<dy,(Tensor -1 Float),,single> Arg<mask,(Tensor -1 Bool),,single> Arg<ratio,(Tensor -1 Float),,optional> 
(edef com.microsoft.TrainableDropoutGrad (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Bool) (Tensor -1 Float)))
(edef com.microsoft.TrainableDropoutGrad (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Bool)))
; shape$com.microsoft.TrainableDropoutGrad


;; Doing com.microsoft.BiasDropout # line "/home/awf/dev/onnxruntime/orttraining/orttraining/core/graph/training_op_defs.cc" 1103
; since_version 1
;; BiasDropout
;; Type constraints:
;; T | ['tensor(float16)', 'tensor(float)', 'tensor(double)'] | Constrain input and output types to float tensors.
;; T1 | ['tensor(float16)', 'tensor(float)', 'tensor(double)'] | Constrain input 'ratio' types to float tensors.
;; T2 | ['tensor(bool)'] | Constrain output 'mask' types to boolean tensors.
; #out=0 ARGS: Arg<data,(Tensor -1 Float),,single> Arg<bias,(Tensor -1 Float),,single> Arg<residual,(Tensor -1 Float),,optional> Arg<ratio,(Tensor -1 Float),,optional> Arg<training_mode,(Tensor -1 Bool),,optional> Attr<Optional,seed,**MISSING**> 
(edef take1$com.microsoft.BiasDropout (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) Float (Tensor -1 Bool) Integer))
(edef take1$com.microsoft.BiasDropout (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) Float Integer))
(edef take1$com.microsoft.BiasDropout (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) Integer))
(edef take1$com.microsoft.BiasDropout (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Float) Integer))
;; NOTE: multiple outputs as tuple
; #out=1 ARGS: Arg<data,(Tensor -1 Float),,single> Arg<bias,(Tensor -1 Float),,single> Arg<residual,(Tensor -1 Float),,optional> Arg<ratio,(Tensor -1 Float),,optional> Arg<training_mode,(Tensor -1 Bool),,optional> Attr<Optional,seed,**MISSING**> 
(edef com.microsoft.BiasDropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) Float (Tensor -1 Bool) Integer))
(edef com.microsoft.BiasDropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) Float Integer))
(edef com.microsoft.BiasDropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) (Tensor -1 Float) (Tensor -1 Float) Integer))
(edef com.microsoft.BiasDropout (Tuple (Tensor -1 Float) (Tensor -1 Bool)) ((Tensor -1 Float) (Tensor -1 Float) Integer))
; shape$com.microsoft.BiasDropout



;; Doing Cast # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 88
;; 
;; The operator casts the elements of a given input tensor to a data type
;; specified by the 'to' argument and returns an output tensor of the same size in
;; the converted type. The 'to' argument must be one of the data types specified
;; in the 'DataType' enum field in the TensorProto message.
;; 
; [see https://github.com/onnx/onnx/blob/2cee65c7eaa4b4fa8f6e23ef7e4b546395b9b7f1/onnx/onnx.in.proto#L116]
;;
;; Casting from string tensor in plain (e.g., "3.14" and "1000") and scientific numeric representations
;; (e.g., "1e-5" and "1E8") to float types is supported. For example, converting string "100.5" to an integer may
;; result 100. There are some string literals reserved for special floating-point values;
;; "+INF" (and "INF"), "-INF", and "NaN" are positive infinity, negative infinity, and not-a-number, respectively.
;; Any string which can exactly match "+INF" in a case-insensitive way would be mapped to positive infinite. Similarly,
;; this case-insensitive rule is applied to "INF" and "NaN". When casting from numeric tensors
;; to string tensors, plain floating-point representation (such as "314.15926") would be used. 
;; Converting non-numerical-literal string such as "Hello World!" is an undefined behavior. Cases 
;; of converting string representing floating-point arithmetic value, such as "2.718", to INT is an undefined behavior.
;; 
;; Conversion from a numerical type to any numerical type is always allowed.
;; User must be aware of precision loss and value change caused by range difference between two types.
;; For example, a 64-bit float 3.1415926459 may be round to a 32-bit float 3.141592. Similarly, converting
;; an integer 36 to Boolean may produce 1 because we truncate bits which can't be stored in the targeted type.
;; Type constraints:
;; T1 | ['tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(int8)', 'tensor(int16)', 'tensor(int32)', 'tensor(int64)', 'tensor(uint8)', 'tensor(uint16)', 'tensor(uint32)', 'tensor(uint64)', 'tensor(bool)', 'tensor(string)'] | Constrain input types. Casting from complex is not supported.
;; T2 | ['tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(int8)', 'tensor(int16)', 'tensor(int32)', 'tensor(int64)', 'tensor(uint8)', 'tensor(uint16)', 'tensor(uint32)', 'tensor(uint64)', 'tensor(bool)', 'tensor(string)'] | Constrain output types. Casting to complex is not supported.
;; WARN: multiple types but no type constraints at Cast: {('', Type.Vec(Type.Float)), ('', Type.Vec(Type.String)), ('', Type.Vec(Type.Integer)), ('', Type.Vec(Type.Bool))}

;; Special-cased in onnx2ks as output type depends on a potentially runtime value...

(edef Cast_FLOATS (Tensor -1 Float) ((Tensor -1 Bool)))
(edef Cast_FLOATS (Tensor -1 Float) ((Tensor -1 String)))
(edef Cast_FLOATS (Tensor -1 Float) ((Tensor -1 Integer)))
(edef Cast_FLOATS (Tensor -1 Float) ((Tensor -1 Float)))

(edef Cast_INTS (Tensor -1 Integer) ((Tensor -1 Bool)))
(edef Cast_INTS (Tensor -1 Integer) ((Tensor -1 String)))
(edef Cast_INTS (Tensor -1 Integer) ((Tensor -1 Integer)))
(edef Cast_INTS (Tensor -1 Integer) ((Tensor -1 Float)))

(edef Cast_FLOAT Float ((Tensor -1 Bool)))
(edef Cast_FLOAT Float ((Tensor -1 String)))
(edef Cast_FLOAT Float ((Tensor -1 Integer)))
(edef Cast_FLOAT Float ((Tensor -1 Float)))

(edef Cast_INT Integer ((Tensor -1 Bool)))
(edef Cast_INT Integer ((Tensor -1 String)))
(edef Cast_INT Integer ((Tensor -1 Integer)))
(edef Cast_INT Integer ((Tensor -1 Float)))


;; Doing Reshape # line "/home/awf/dev/onnxruntime/cmake/external/onnx/onnx/defs/tensor/defs.cc" 262
; since_version 13
;; 
;; Reshape the input tensor similar to numpy.reshape.
;; First input is the data tensor, second input is a shape tensor which specifies the output shape. It outputs the reshaped tensor.
;; At most one dimension of the new shape can be -1. In this case, the value is
;; inferred from the size of the tensor and the remaining dimensions. A dimension
;; could also be 0, in which case the actual dimension value is unchanged (i.e. taken
;; from the input tensor).
;; Type constraints:
;; T | ['tensor(uint8)', 'tensor(uint16)', 'tensor(uint32)', 'tensor(uint64)', 'tensor(int8)', 'tensor(int16)', 'tensor(int32)', 'tensor(int64)', 'tensor(bfloat16)', 'tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(string)', 'tensor(bool)', 'tensor(complex64)', 'tensor(complex128)'] | Constrain input and output types to all tensor types.
;; NOTE: output mangler cplx$
; #out=0 ARGS: Arg<data,(Tensor -1 (Tuple Float Float)),cplx$,single> Arg<shape,(Tensor -1 Integer),,single> 
(edef Reshape (Tensor -1 (Tuple Float Float)) ((Tensor -1 (Tuple Float Float)) (Tensor 1 Integer)))
(edef Reshape (Tensor -1 Integer)             ((Tensor -1 Integer) (Tensor 1 Integer)))
(edef Reshape (Tensor -1 String)              ((Tensor -1 String) (Tensor 1 Integer)))
(edef Reshape (Tensor -1 Bool)                ((Tensor -1 Bool) (Tensor 1 Integer)))
(edef Reshape (Tensor -1 Float)               ((Tensor -1 Float) (Tensor 1 Integer)))


;; Doing Unsqueeze # line "/home/awf/dev/onnxruntime/cmake/external/onnx/onnx/defs/tensor/defs.cc" 1763
; since_version 13
;; 
;; Insert single-dimensional entries to the shape of an input tensor (`data`).
;; Takes one required input `axes` - which contains a list of dimension indices and this operator will insert a dimension of value `1` into the corresponding index of the output tensor (`expanded`).
;; 
;; For example:
;;   Given an input tensor (`data`) of shape [3, 4, 5], then
;;   Unsqueeze(data, axes=[0, 4]) outputs a tensor (`expanded`) containing same data as `data` but with shape [1, 3, 4, 5, 1].
;; 
;; The input `axes` should not contain any duplicate entries. It is an error if it contains duplicates.
;; The rank of the output tensor (`output_rank`) is the rank of the input tensor (`data`) plus the number of values in `axes`.
;; Each value in `axes` should be within the (inclusive) range [-output_rank , output_rank - 1]. 
;; The order of values in `axes` does not matter and can come in any order. 
;; 
;; Type constraints:
;; T | ['tensor(uint8)', 'tensor(uint16)', 'tensor(uint32)', 'tensor(uint64)', 'tensor(int8)', 'tensor(int16)', 'tensor(int32)', 'tensor(int64)', 'tensor(bfloat16)', 'tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(string)', 'tensor(bool)', 'tensor(complex64)', 'tensor(complex128)'] | Constrain input and output types to all tensor types.
;; NOTE: output mangler cplx$
;(edef Unsqueeze (Tensor -1 (Tuple Float Float)) ((Tensor -1 (Tuple Float Float)) (Tensor -1 Integer)))
(edef Unsqueeze (Tensor -1 Integer) ((Tensor -1 Integer) (Tensor 1 Integer)))
;(edef Unsqueeze (Tensor -1 String) ((Tensor -1 String) (Tensor -1 Integer)))
;(edef Unsqueeze (Tensor -1 Bool) ((Tensor -1 Bool) (Tensor -1 Integer)))
;(edef Unsqueeze (Tensor -1 Float) ((Tensor -1 Float) (Tensor -1 Integer)))



;; Doing If # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/controlflow/defs.cc" 423
;; If conditional
;; Type constraints:
;; V | ['tensor(uint8)', 'tensor(uint16)', 'tensor(uint32)', 'tensor(uint64)', 'tensor(int8)', 'tensor(int16)', 'tensor(int32)', 'tensor(int64)', 'tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(string)', 'tensor(bool)', 'tensor(complex64)', 'tensor(complex128)'] | All Tensor types
;; B | ['tensor(bool)'] | Only bool
;; WARN: multiple types but no type constraints at If: {('', Type.Vec(Type.Float)), ('', Type.Vec(Type.Integer)), ('', Type.Vec(Type.String)), ('', Type.Vec(Type.Bool)), ('cplx$', Type.Vec(Type.Tuple(Type.Float, Type.Float)))}
(edef If (Tensor -1 Float) ((Tensor -1 Bool) (Lam (Tensor -1 Float) (Tensor -1 Float)) (Lam (Tensor -1 Float) (Tensor -1 Float))))

;; Doing Loop # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/controlflow/defs.cc" 614
;; 
;; Generic Looping construct. This loop has multiple termination conditions:
;; 
;; 1) Trip count. Iteration count specified at runtime. Set by
;;    specifying the input M. Optional. Set to empty string to omit.
;;    Note that a static trip count (specified at graph construction time) can be
;;    specified by passing in a constant node for input M.
;; 2) Loop termination condition. This is an input to the op that determines
;;    whether to run the first iteration and also a loop-carried dependency for
;;    the body graph. The body graph must yield a value for the condition variable,
;;    whether this input is provided or not.
;; 
;; This table summarizes the operating modes of this operator with equivalent
;; C-style code:
;; 
;;     Operator inputs defined as (max_trip_count, condition_var).
;; 
;;     input ("", ""):
;;         for (int i=0; ; ++i) {
;;           cond = ... // Note this value is ignored, but is required in the body
;;         }
;; 
;;     input ("", cond) // Note this is analogous to a while loop
;;         bool cond = ...;
;;         for (int i=0; cond; ++i) {
;;           cond = ...;
;;         }
;; 
;;     input ("", 1) // Note this is analogous to a do-while loop
;;         bool cond = true
;;         for (int i=0; cond; ++i) {
;;           cond = ...;
;;         }
;; 
;;     input (trip_count, "") // Note this is analogous to a for loop
;;         int trip_count = ...
;;         for (int i=0; i < trip_count; ++i) {
;;           cond = ...; // ignored
;;         }
;; 
;;     input (trip_count, cond)
;;         int trip_count = ...;
;;         bool cond = ...;
;;         for (int i=0; i < trip_count && cond; ++i) {
;;           cond = ...;
;;         }
;; 
;; 
;; *Sample usage - cond as well as trip count*
;; 
;;     graph predict-net {
;;       %a = Constant[value = <Scalar Tensor [3]>]()
;;       %b = Constant[value = <Scalar Tensor [6]>]()
;;       %keepgoing = Constant[value = <Scalar Tensor [1]>]()
;;       %max_trip_count = Constant[value = <Scalar Tensor [10]>]()
;;       %keepgoing_out, %b_out, %user_defined_vals = Loop[body = <graph body-net>](%max_trip_count, %keepgoing, %b)
;;       return
;;     }
;; 
;;     graph body-net (
;;       %i[INT32, scalar]           // iteration number
;;       %keepgoing_in[BOOL, scalar] // incoming loop-termination-condition; not used
;;       %b_in[INT32, scalar]        // incoming value of loop-carried-dependency b
;;     ) {
;;       %my_local = Add(%a, %b_in)
;;       %b_out = Sub(%a, %b_in) // outgoing value of loop-carried-dependency b
;;       %keepgoing_out = Greater(%my_local, %b_out) // outgoing loop-termination-condition
;;       %user_defined_val = Add(%b_in, %b_in) // scan-output value to be accumulated
;;       return %keepgoing_out, %b_out, %user_defined_val
;;     }
;; 
;; *Sample equivalent C code*
;; 
;;     {
;;       /* User-defined code (enclosing scope) */
;;       int a = 3, b = 6;
;;       bool keepgoing = true; // Analogous to input cond
;;       /* End user-defined code */
;; 
;;       /* Implicitly-defined code */
;;       const int max_trip_count = 10; // Analogous to input M
;;       int user_defined_vals[]; // Imagine this is resizable
;;       /* End implicitly-defined code */
;;       /* initialize loop-carried variables and scan-output variables */
;;       bool keepgoing_out = keepgoing
;;       int b_out = b
;; 
;;       for (int i=0; i < max_trip_count && keepgoing_out; ++i) {
;;         /* Implicitly-defined code: bind actual parameter values
;;            to formal parameter variables of loop-body */
;;         bool keepgoing_in = keepgoing_out; 
;;         bool b_in = b_out;
;; 
;;         /* User-defined code (loop body) */
;;         int my_local = a + b_in; // Reading value "a" from the enclosing scope is fine
;;         b_out = a - b_in;
;;         keepgoing_out = my_local > b_out; 
;;         user_defined_val = b_in + b_in; // b_in and b_out are different variables
;;         /* End user-defined code */
;; 
;;         /* Implicitly defined-code */
;;         user_defined_vals[i] = user_defined_val // accumulate scan-output values
;;       }
;;       // int t = my_local; // Can't do this. my_local is not accessible here.
;; 
;;       // The values below are bound to the output variables of the loop and therefore accessible
;;       // b_out; user_defined_vals; keepgoing_out;
;;     }
;; 
;; There are several things of note in this code snippet:
;; 
;; 1) Values from the enclosing scope (i.e. variable "a" here) are in scope and can
;;    be referenced in the inputs of the loop.
;; 2) Any values computed in the loop body that needs to be used in a subsequent
;;    iteration or after the loop are modelled using a pair of variables in the loop-body,
;;    consisting of an input variable (eg., b_in) and an output variable (eg., b_out).
;;    These are referred to as loop-carried dependences. The loop operation node
;;    supplies the input value of the input variable for the first iteration, and
;;    returns the output value of the output variable produced by the final
;;    iteration.
;; 3) Scan_output variables are used to implicitly concatenate values computed across
;;    all the iterations. In the above example, the value of user_defined_val computed
;;    over all iterations are concatenated and returned as the value of user_defined_vals
;;    after the loop.
;; 4) Values created in the body cannot be accessed in the enclosing scope,
;;    except using the mechanism described above.
;; 
;; Note that the semantics of this op support "diagonal" or "wavefront" execution.
;; (See Step 3 here for an example:
;; https://devblogs.nvidia.com/optimizing-recurrent-neural-networks-cudnn-5/).
;; Frontends should emit multi-layer RNNs as a series of While operators (with
;; time being the inner looping dimension), with each successive layer consuming
;; the scan_outputs from the previous layer, possibly going through several
;; point-wise operators (e.g. dropout, residual connections, linear layer).
;; Type constraints:
;; V | ['tensor(uint8)', 'tensor(uint16)', 'tensor(uint32)', 'tensor(uint64)', 'tensor(int8)', 'tensor(int16)', 'tensor(int32)', 'tensor(int64)', 'tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(string)', 'tensor(bool)', 'tensor(complex64)', 'tensor(complex128)'] | All Tensor types
;; I | ['tensor(int64)'] | tensor of int64, which should be a scalar.
;; B | ['tensor(bool)'] | tensor of bool, which should be a scalar.
; ;; NOTE: manglers {'cplx$'}
; (edef Loop (Tensor -1 String) ((Tensor -1 Integer) (Tensor -1 Bool) (Tensor -1 String) (Lam None None)))
; ;; NOTE: manglers {'cplx$'}
; (edef Loop (Tensor -1 Float) ((Tensor -1 Integer) (Tensor -1 Bool) (Tensor -1 Float) (Lam None None)))
; ;; NOTE: manglers {'cplx$'}
; (edef Loop (Tensor -1 Integer) ((Tensor -1 Integer) (Tensor -1 Bool) (Tensor -1 Integer) (Lam None None)))
; ;; NOTE: output mangler cplx$
; ;; NOTE: manglers {'cplx$'}
; (edef Loop (Tensor -1 (Tuple Float Float)) ((Tensor -1 Integer) (Tensor -1 Bool) (Tensor -1 (Tuple Float Float)) (Lam None None)))
; ;; NOTE: manglers {'cplx$'}
; (edef Loop (Tensor -1 Bool) ((Tensor -1 Integer) (Tensor -1 Bool) (Tensor -1 Bool) (Lam None None)))

;; Doing Scan # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/controlflow/defs.cc" 811
;; 
;; Scan can be used to iterate over one or more scan_input tensors,
;; constructing zero or more scan_output tensors. It combines ideas from general recurrences,
;; functional programming constructs such as scan, fold, map, and zip and is intended to enable
;; generalizations of RNN-like constructs for sequence-to-sequence processing.
;; Other tensors (referred to as state_variables here) can be used to carry a state
;; when iterating from one element to another (similar to hidden-state in RNNs, also referred
;; to as loop-carried dependences in the context of loops).
;; Many common usages involve a single scan_input tensor (where functionality
;; similar to scan, fold and map can be obtained). When more than one scan_input is used,
;; a behavior similar to zip is obtained.
;; 
;; The attribute body must be a graph, specifying the computation to be performed in
;; every iteration. It takes as input the current values of the state_variables and
;; the current iterated element of the scan_inputs. It must return the (updated) values
;; of the state_variables and zero or more scan_output_element tensors. The values of the
;; scan_output_element tensors are concatenated over all the iterations to produce the
;; scan_output values of the scan construct (similar to the concatenated intermediate
;; hidden-state values of RNN-like constructs). All the output tensors (state_variables as
;; well as scan_output_element tensors) are required to have the same shape in each iteration
;; of the loop (a restriction imposed to enable efficient memory allocation).
;; 
;; Note that the iterated element passed to the body subgraph does not have a sequence
;; axis. It will have a rank one less than the rank of the corresponding scan_input.
;; 
;; The scan operation returns the final values of the state_variables as well as the
;; scan_outputs.
;; 
;; The optional attribute scan_input_directions specifies the direction (forward or backward)
;; for each scan input. If this attribute is omitted, all sequences are scanned in the forward
;; direction. A bidirectional scan may be performed by specifying the same tensor input twice
;; in the scan_inputs, once with a forward direction, and once with a backward direction.
;; 
;; The scan_output of the operation is produced by concatenating the scan_output_element
;; values produced by the body in each iteration.  The optional attribute scan_output_directions
;; specifies the direction in which scan_output is constructed (by appending or prepending the
;; scan_output_element to scan_output in each iteration) for each scan_output. If this attribute
;; is omitted, the scan_output_element is appended to the scan_output in each iteration.
;; 
;; The optional attribute scan_input_axes specifies the axis to be scanned for each scan_input.
;; If omitted, every scan_input will be scanned in axis 0. For example, if axis 0 is the
;; batch axis and axis 1 is the time axis (to be scanned), specify an axis value of 1.
;; Note that scanning a non-zero axis may be less efficient than scanning axis zero.
;; 
;; The optional attribute scan_output_axes specifies the axis along which the scan_outputs
;; are accumulated for each scan_output. For example, if axis 1 is the time axis (to be
;; scanned) for both inputs and outputs, specify a scan_input axis and scan_output axis
;; value of 1.
;; 
;; Note that because of the ONNX restriction that only the last parameter of an operator can
;; be variadic, the initial-states and scan-inputs are listed together as one input parameter.
;; Similarly, the final-states and scan-outputs are listed together as one output parameter.
;; The attribute num_scan_inputs indicates the number M of scan-inputs.
;; 
;; The behavior of
;; 
;;     Scan <
;;         num_scan_inputs = m,
;;         body = loop-body,
;;         scan_input_axes = [axis_1, ..., axis_m]
;;     > (init_1, ..., init_n, scan_1, ..., scan_m)
;; 
;; is equivalent to the following pseudo-code:
;; 
;;     // scan_i.shape[axis_i] denotes the (max) sequence-length of scan_i
;;     // scan_i.shape[axis_i] is required to be equal to scan_j.shape[axis_j] for all i,j.
;;     sequence_length = scan_1.shape[axis_1];
;; 
;;     // initialize state-variables
;;     st_1 = init_1; ... st_n = init_n;
;;     // initialize scan-output variables: [] denotes an empty tensor
;;     scan_out_1 = []; ...; scan_out_k = [];
;;     // identify number of iterations:
;; 
;;     // execute loop
;;     for (int t = 0; t < sequence_length; ++t) {
;;         // generate the scan-input elements: the notation T<axis=k>[t] indicates the sub-tensor
;;         // of rank one less than T obtained by indexing T at position t along axis k.
;;         si_1 = scan_1<axis=axis_1>[t];
;;         ... ;
;;         si_m = scan_m<axis=axis_m>[t];
;;         // execute loop-body
;;         st_1, ..., st_n, so_1, ..., so_k = loop-body(st_1, ..., st_n, si_1, ..., si_m)
;;         // accumulate the scan-output elements
;;         scan_out_1 = Concat<axis=0>(scan_out_1, so_1); ... ; scan_out_k = Concat<axis=0>(scan_out_k, so_k);
;;     }
;; 
;;     return st_1, ..., st_n, scan_out_1, ..., scan_out_k;
;; 
;; *Sample usage: Encoding RNN using a Scan*
;; 
;; The following example shows how a simple RNN over an input tensor %X, with weight tensor %Wi,
;; recurrence weight tensor %Ri, bias tensors %Wbi and %Rbi, and initial hidden-state %H_0 can
;; be encoded as a ScanLoop. Note that the loop-body is a nested graph, and it directly computes
;; %Wi, %Ri, %Wbi, and %Rbi (typically constants or initializers in the body graph). If these
;; values are computed in the outer graph, they need to be passed in as extra state_variables.
;; 
;;     graph rnn-encoding {
;;       %H_0 = ... 
;;       %X = ...
;;       %Y_h, %Y = Scan[body = <graph rnn-cell-1>, num_scan_inputs=1](%H_0, %X)
;;       return %Y, %Y_h
;;     }
;; 
;;     graph rnn-cell-1 (
;;       %H_tminus1[FLOAT, tensor]
;;       %X_t[FLOAT, tensor]
;;     ) {
;;       %Wi = ...
;;       %Ri = ...
;;       %Wbi = ...
;;       %Rbi = ...
;;       %t1 = X_t * (Wi^T)
;;       %t2 = H_tminus1*(Ri^T)
;;       %t3 = Add(%t1, %t2)
;;       %t4 = Add(%t3, %Wbi)
;;       %t5 = Add(%t4, %Rbi)
;;       %Ht = Tanh(%t5)
;;       %Accumulate = Identity(%Ht)
;;       return %Ht, %Accumulate
;;     }
;; 
;; Type constraints:
;; I | ['tensor(int64)'] | Int64 tensor
; ;; V | ['tensor(uint8)', 'tensor(uint16)', 'tensor(uint32)', 'tensor(uint64)', 'tensor(int8)', 'tensor(int16)', 'tensor(int32)', 'tensor(int64)', 'tensor(float16)', 'tensor(float)', 'tensor(double)', 'tensor(string)', 'tensor(bool)', 'tensor(complex64)', 'tensor(complex128)'] | All Tensor types
; ;; NOTE: manglers {'cplx$'}
; (edef Scan (Tensor -1 String) ((Tensor -1 String) (Lam (None) (None)) Integer (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer)))
; ;; NOTE: manglers {'cplx$'}
; (edef Scan (Tensor -1 Integer) ((Tensor -1 Integer) (Lam (None) (None)) Integer (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer)))
; ;; NOTE: output mangler cplx$
; ;; NOTE: manglers {'cplx$'}
; (edef Scan (Tensor -1 (Tuple Float Float)) ((Tensor -1 (Tuple Float Float)) (Lam (None) (None)) Integer (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer)))
; ;; NOTE: manglers {'cplx$'}
; (edef Scan (Tensor -1 Bool) ((Tensor -1 Bool) (Lam (None) (None)) Integer (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer)))
; ;; NOTE: manglers {'cplx$'}
; (edef Scan (Tensor -1 Float) ((Tensor -1 Float) (Lam (None) (None)) Integer (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer) (Tensor -1 Integer)))


;; Doing TreeEnsembleClassifier # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 1003
;; 
;;     Tree Ensemble classifier.  Returns the top class for each of N inputs.<br>
;;     The attributes named 'nodes_X' form a sequence of tuples, associated by 
;;     index into the sequences, which must all be of equal length. These tuples
;;     define the nodes.<br>
;;     Similarly, all fields prefixed with 'class_' are tuples of votes at the leaves.
;;     A leaf may have multiple votes, where each vote is weighted by
;;     the associated class_weights index.<br>
;;     One and only one of classlabels_strings or classlabels_int64s
;;     will be defined. The class_ids are indices into this list.
;;
;; TC: T1 | ['tensor(float)', 'tensor(double)', 'tensor(int64)', 'tensor(int32)'] | The input type must be a tensor of a numeric type.
;; TC: T2 | ['tensor(string)', 'tensor(int64)'] | The output type will be a tensor of strings or integers, depending on which of the the classlabels_* attributes is used.
;;
;; WARN: multiple types but no type constraints at TreeEnsembleClassifier: {('', Type.Vec(Type.Integer)), ('', Type.Vec(Type.String))}
;; NOTE: multiple outputs as tuple
(edef
  TreeEnsembleClassifier
  (Tuple (Tensor -1 Integer) (Tensor -1 Float))       ;; ints out
  (#|X|# (Tensor -1 Integer)
   #|base_values|# (Tensor -1 Float)
   #|class_ids|# (Tensor -1 Integer)
   #|class_nodeids|# (Tensor -1 Integer)
   #|class_treeids|# (Tensor -1 Integer)
   #|class_weights|# (Tensor -1 Float)
   #|classlabels_int64s|# (Tensor -1 Integer)   ;; ints in
   #|nodes_falsenodeids|# (Tensor -1 Integer)
   #|nodes_featureids|# (Tensor -1 Integer)
   #|nodes_hitrates|# (Tensor -1 Float)
   #|nodes_missing_value_tracks_true|# (Tensor -1 Integer)
   #|nodes_modes|# (Tensor -1 String)
   #|nodes_nodeids|# (Tensor -1 Integer)
   #|nodes_treeids|# (Tensor -1 Integer)
   #|nodes_truenodeids|# (Tensor -1 Integer)
   #|nodes_values|# (Tensor -1 Float)
   #|post_transform|# String))
;; WARN: multiple types but no type constraints at TreeEnsembleClassifier: {('', Type.Vec(Type.Integer)), ('', Type.Vec(Type.String))}
;; NOTE: multiple outputs as tuple
(edef
  TreeEnsembleClassifier
  (Tuple (Tensor -1 String) (Tensor -1 Float))           ;; strings out
  (#|X|# (Tensor -1 Float)
   #|base_values|# (Tensor -1 Float)
   #|class_ids|# (Tensor -1 Integer)
   #|class_nodeids|# (Tensor -1 Integer)
   #|class_treeids|# (Tensor -1 Integer)
   #|class_weights|# (Tensor -1 Float)
   #|classlabels_strings|# (Tensor -1 String)      ;; strings in
   #|nodes_falsenodeids|# (Tensor -1 Integer)
   #|nodes_featureids|# (Tensor -1 Integer)
   #|nodes_hitrates|# (Tensor -1 Float)
   #|nodes_missing_value_tracks_true|# (Tensor -1 Integer)
   #|nodes_modes|# (Tensor -1 String)
   #|nodes_nodeids|# (Tensor -1 Integer)
   #|nodes_treeids|# (Tensor -1 Integer)
   #|nodes_truenodeids|# (Tensor -1 Integer)
   #|nodes_values|# (Tensor -1 Float)
   #|post_transform|# String))


;; Doing LinearClassifier # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 572
;; 
;;     Linear classifier
;; Type constraints:
;; T1 | ['tensor(float)', 'tensor(double)', 'tensor(int64)', 'tensor(int32)'] | The input must be a tensor of a numeric type, and of of shape [N,C] or [C]. In the latter case, it will be treated as [1,C]
;; T2 | ['tensor(string)', 'tensor(int64)'] | The output will be a tensor of strings or integers.
;; WARN: multiple types but no type constraints at LinearClassifier: {('', Type.Vec(Type.String)), ('', Type.Vec(Type.Integer))}
;; NOTE: multiple outputs as tuple
(edef
  LinearClassifier
  (Tuple (Tensor -1 String) (Tensor -1 Float))
  (#|X|# (Tensor -1 Integer)
   #|classlabels_strings|# (Tensor -1 String)
   #|coefficients|# (Tensor -1 Float)
   #|intercepts|# (Tensor -1 Float)
   #|multi_class|# Integer
   #|post_transform|# String))
;; WARN: multiple types but no type constraints at LinearClassifier: {('', Type.Vec(Type.String)), ('', Type.Vec(Type.Integer))}
;; NOTE: multiple outputs as tuple
(edef
  LinearClassifier
  (Tuple (Tensor -1 Integer) (Tensor -1 Float))
  (#|X|# (Tensor -1 Float)
   #|classlabels_ints|# (Tensor -1 Integer)
   #|coefficients|# (Tensor -1 Float)
   #|intercepts|# (Tensor -1 Float)
   #|multi_class|# Integer
   #|post_transform|# String))

;; Doing SVMClassifier # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 811
;; 
;;     Support Vector Machine classifier
;; Type constraints:
;; T1 | ['tensor(float)', 'tensor(double)', 'tensor(int64)', 'tensor(int32)'] | The input must be a tensor of a numeric type, either [C] or [N,C].
;; T2 | ['tensor(string)', 'tensor(int64)'] | The output type will be a tensor of strings or integers, depending on which of the the classlabels_* attributes is used. Its size will match the bactch size of the input.
(edef
  SVMClassifier
  (Tuple (Tensor -1 String) (Tensor -1 Float))
  (#|X|# (Tensor -1 Integer)
   #|classlabels_strings|# (Tensor -1 String)
   #|coefficients|# (Tensor -1 Float)
   #|kernel_params|# (Tensor -1 Float)
   #|kernel_type|# String
   #|post_transform|# String
   #|prob_a|# (Tensor -1 Float)
   #|prob_b|# (Tensor -1 Float)
   #|rho|# (Tensor -1 Float)
   #|support_vectors|# (Tensor -1 Float)
   #|vectors_per_class|# (Tensor -1 Integer)))

(edef
  SVMClassifier
  (Tuple (Tensor -1 Integer) (Tensor -1 Float))
  (#|X|# (Tensor -1 Float)
   #|classlabels_ints|# (Tensor -1 Integer)
   #|coefficients|# (Tensor -1 Float)
   #|kernel_params|# (Tensor -1 Float)
   #|kernel_type|# String
   #|post_transform|# String
   #|prob_a|# (Tensor -1 Float)
   #|prob_b|# (Tensor -1 Float)
   #|rho|# (Tensor -1 Float)
   #|support_vectors|# (Tensor -1 Float)
   #|vectors_per_class|# (Tensor -1 Integer)))

;; Doing ZipMap # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 1166
;; 
;;     Creates a map from the input and the attributes.<br>
;;     The values are provided by the input tensor, while the keys are specified by the attributes.
;;     Must provide keys in either classlabels_strings or classlabels_int64s (but not both).<br>
;;     The columns of the tensor correspond one-by-one to the keys specified by the attributes. There must be as many columns as keys.<br>
;; Type constraints:
;; T | ['seq(map(string, float))', 'seq(map(int64, float))'] | The output will be a sequence of string or integer maps to float.
;; WARN: multiple types but no type constraints at ZipMap: {('seq$map$', Type.Vec(Type.Vec(Type.Tuple(Type.String, Type.Vec(Type.Float))))), ('seq$map$', Type.Vec(Type.Vec(Type.Tuple(Type.Integer, Type.Vec(Type.Float)))))}
;; NOTE: output mangler seq$map$
(edef
  ZipMap
  (Tensor -1 (Tensor -1 (Tuple String (Tensor -1 Float))))
  (#|X|# (Tensor -1 Float)
   #|classlabels_strings|# (Tensor -1 String)))
(edef
  ZipMap
  (Tensor -1 (Tensor -1 (Tuple Integer (Tensor -1 Float))))
  (#|X|# (Tensor -1 Float)
   #|classlabels_int64s|# (Tensor -1 Integer)))
