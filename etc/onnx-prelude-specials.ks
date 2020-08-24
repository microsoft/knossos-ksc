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
  (Tuple (Vec Integer) (Vec Float))       ;; ints out
  ((X : (Vec Integer))
   (base_values : (Vec Float))
   (class_ids : (Vec Integer))
   (class_nodeids : (Vec Integer))
   (class_treeids : (Vec Integer))
   (class_weights : (Vec Float))
   (classlabels_int64s : (Vec Integer))   ;; ints in
   (nodes_falsenodeids : (Vec Integer))
   (nodes_featureids : (Vec Integer))
   (nodes_hitrates : (Vec Float))
   (nodes_missing_value_tracks_true : (Vec Integer))
   (nodes_modes : (Vec String))
   (nodes_nodeids : (Vec Integer))
   (nodes_treeids : (Vec Integer))
   (nodes_truenodeids : (Vec Integer))
   (nodes_values : (Vec Float))
   (post_transform : String)))
;; WARN: multiple types but no type constraints at TreeEnsembleClassifier: {('', Type.Vec(Type.Integer)), ('', Type.Vec(Type.String))}
;; NOTE: multiple outputs as tuple
(edef
  TreeEnsembleClassifier
  (Tuple (Vec String) (Vec Float))           ;; strings out
  ((X : (Vec Float))
   (base_values : (Vec Float))
   (class_ids : (Vec Integer))
   (class_nodeids : (Vec Integer))
   (class_treeids : (Vec Integer))
   (class_weights : (Vec Float))
   (classlabels_strings : (Vec String))      ;; strings in
   (nodes_falsenodeids : (Vec Integer))
   (nodes_featureids : (Vec Integer))
   (nodes_hitrates : (Vec Float))
   (nodes_missing_value_tracks_true : (Vec Integer))
   (nodes_modes : (Vec String))
   (nodes_nodeids : (Vec Integer))
   (nodes_treeids : (Vec Integer))
   (nodes_truenodeids : (Vec Integer))
   (nodes_values : (Vec Float))
   (post_transform : String)))


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
  (Tuple (Vec String) (Vec Float))
  ((X : (Vec Integer))
   (classlabels_strings : (Vec String))
   (coefficients : (Vec Float))
   (intercepts : (Vec Float))
   (multi_class : Integer)
   (post_transform : String)))
;; WARN: multiple types but no type constraints at LinearClassifier: {('', Type.Vec(Type.String)), ('', Type.Vec(Type.Integer))}
;; NOTE: multiple outputs as tuple
(edef
  LinearClassifier
  (Tuple (Vec Integer) (Vec Float))
  ((X : (Vec Float))
   (classlabels_ints : (Vec Integer))
   (coefficients : (Vec Float))
   (intercepts : (Vec Float))
   (multi_class : Integer)
   (post_transform : String)))

;; Doing SVMClassifier # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 811
;; 
;;     Support Vector Machine classifier
;; Type constraints:
;; T1 | ['tensor(float)', 'tensor(double)', 'tensor(int64)', 'tensor(int32)'] | The input must be a tensor of a numeric type, either [C] or [N,C].
;; T2 | ['tensor(string)', 'tensor(int64)'] | The output type will be a tensor of strings or integers, depending on which of the the classlabels_* attributes is used. Its size will match the bactch size of the input.
(edef
  SVMClassifier
  (Tuple (Vec String) (Vec Float))
  ((X : (Vec Integer))
   (classlabels_strings : (Vec String))
   (coefficients : (Vec Float))
   (kernel_params : (Vec Float))
   (kernel_type : String)
   (post_transform : String)
   (prob_a : (Vec Float))
   (prob_b : (Vec Float))
   (rho : (Vec Float))
   (support_vectors : (Vec Float))
   (vectors_per_class : (Vec Integer))))

(edef
  SVMClassifier
  (Tuple (Vec Integer) (Vec Float))
  ((X : (Vec Float))
   (classlabels_ints : (Vec Integer))
   (coefficients : (Vec Float))
   (kernel_params : (Vec Float))
   (kernel_type : String)
   (post_transform : String)
   (prob_a : (Vec Float))
   (prob_b : (Vec Float))
   (rho : (Vec Float))
   (support_vectors : (Vec Float))
   (vectors_per_class : (Vec Integer))))

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
  (Vec (Vec (Tuple String (Vec Float))))
  ((X : (Vec Float))
   (classlabels_strings : (Vec String))))
(edef
  ZipMap
  (Vec (Vec (Tuple Integer (Vec Float))))
  ((X : (Vec Float))
   (classlabels_int64s : (Vec Integer))))

;; Doing Cast # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 88
;; 
;; The operator casts the elements of a given input tensor to a data type
;; specified by the 'to' argument and returns an output tensor of the same size in
;; the converted type. The 'to' argument must be one of the data types specified
;; in the 'DataType' enum field in the TensorProto message.
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
(edef Cast (Vec Float) ((input : (Vec Bool)) (to : Integer)))
;; WARN: multiple types but no type constraints at Cast: {('', Type.Vec(Type.Float)), ('', Type.Vec(Type.String)), ('', Type.Vec(Type.Integer)), ('', Type.Vec(Type.Bool))}
(edef Cast (Vec Float) ((input : (Vec String)) (to : Integer)))
;; WARN: multiple types but no type constraints at Cast: {('', Type.Vec(Type.Float)), ('', Type.Vec(Type.String)), ('', Type.Vec(Type.Integer)), ('', Type.Vec(Type.Bool))}
(edef Cast (Vec Float) ((input : (Vec Integer)) (to : Integer)))
;; WARN: multiple types but no type constraints at Cast: {('', Type.Vec(Type.Float)), ('', Type.Vec(Type.String)), ('', Type.Vec(Type.Integer)), ('', Type.Vec(Type.Bool))}
(edef Cast (Vec Float) ((input : (Vec Float)) (to : Integer)))
