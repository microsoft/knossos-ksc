;; Doing Adam # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/training/defs.cc" 789
(edef
  Adam
  (Vec Float)
  ((R : (Vec Float))
   (T : (Vec Integer))
   (inputs : (Vec Float))
   (alpha : Float)
   (beta : Float)
   (epsilon : Float)
   (norm_coefficient : Float)
   (norm_coefficient_post : Float)))
;; Doing Adagrad # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/training/defs.cc" 462
(edef
  Adagrad
  (Vec Float)
  ((R : (Vec Float))
   (T : (Vec Integer))
   (inputs : (Vec Float))
   (decay_factor : Float)
   (epsilon : Float)
   (norm_coefficient : Float)))
;; Doing Momentum # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/training/defs.cc" 612
(edef
  Momentum
  (Vec Float)
  ((R : (Vec Float))
   (T : (Vec Integer))
   (inputs : (Vec Float))
   (alpha : Float)
   (beta : Float)
   (mode : String)
   (norm_coefficient : Float)))
;; Doing GraphCall # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/training/defs.cc" 325
(edef
  GraphCall
  (Vec Bool)
  ((Inputs : (Vec Bool)) (graph_name : String)))
(edef
  GraphCall
  (Vec String)
  ((Inputs : (Vec String)) (graph_name : String)))
(edef
  GraphCall
  (Vec Float)
  ((Inputs : (Vec Float)) (graph_name : String)))
(edef
  GraphCall
  (Vec Integer)
  ((Inputs : (Vec Integer)) (graph_name : String)))
(edef
  GraphCall
  (Vec (Tuple Float Float))
  ((Inputs : (Vec (Tuple Float Float))) (graph_name : String)))
;; Doing LinearRegressor # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 620
(edef
  LinearRegressor
  None
  ((X : (Vec Float))
   (coefficients : (Vec Float))
   (intercepts : (Vec Float))
   (post_transform : String)
   (targets : Integer)))
(edef
  LinearRegressor
  None
  ((X : (Vec Integer))
   (coefficients : (Vec Float))
   (intercepts : (Vec Float))
   (post_transform : String)
   (targets : Integer)))
;; Doing Imputer # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 316
(edef
  Imputer
  (Vec Float)
  ((X : (Vec Float))
   (imputed_value_floats : (Vec Float))
   (imputed_value_int64s : (Vec Integer))
   (replaced_value_float : Float)
   (replaced_value_int64 : Integer)))
(edef
  Imputer
  (Vec Integer)
  ((X : (Vec Integer))
   (imputed_value_floats : (Vec Float))
   (imputed_value_int64s : (Vec Integer))
   (replaced_value_float : Float)
   (replaced_value_int64 : Integer)))
;; Doing FeatureVectorizer # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 270
(edef
  FeatureVectorizer
  None
  ((X : (Vec Float)) (inputdimensions : (Vec Integer))))
(edef
  FeatureVectorizer
  None
  ((X : (Vec Integer)) (inputdimensions : (Vec Integer))))
;; Doing DictVectorizer # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 234
(edef
  DictVectorizer
  (Vec String)
  ((X : (Vec (Tuple Integer String)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec Float)
  ((X : (Vec (Tuple Integer String)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec Integer)
  ((X : (Vec (Tuple Integer String)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec String)
  ((X : (Vec (Tuple String Integer)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec Float)
  ((X : (Vec (Tuple String Integer)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec Integer)
  ((X : (Vec (Tuple String Integer)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec String)
  ((X : (Vec (Tuple String Float)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec Float)
  ((X : (Vec (Tuple String Float)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec Integer)
  ((X : (Vec (Tuple String Float)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec String)
  ((X : (Vec (Tuple Integer Float)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec Float)
  ((X : (Vec (Tuple Integer Float)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
(edef
  DictVectorizer
  (Vec Integer)
  ((X : (Vec (Tuple Integer Float)))
   (int64_vocabulary : (Vec Integer))
   (string_vocabulary : (Vec String))))
;; Doing Binarizer # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 56
(edef Binarizer (Vec Float) ((X : (Vec Float)) (threshold : Float)))
(edef
  Binarizer
  (Vec Integer)
  ((X : (Vec Integer)) (threshold : Float)))
;; Doing ArrayFeatureExtractor # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 32
(edef
  ArrayFeatureExtractor
  (Vec String)
  ((X : (Vec String)) (Y : None)))
(edef ArrayFeatureExtractor (Vec Float) ((X : (Vec Float)) (Y : None)))
(edef
  ArrayFeatureExtractor
  (Vec Integer)
  ((X : (Vec Integer)) (Y : None)))
;; Doing GreaterOrEqual # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 244
;; has body: <class 'onnx.onnx_operators_ml_pb2.FunctionProto'>
(edef GreaterOrEqual (Vec Bool) ((A : (Vec Float)) (B : (Vec Float))))
;; has body: <class 'onnx.onnx_operators_ml_pb2.FunctionProto'>
(edef
  GreaterOrEqual
  (Vec Bool)
  ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing Celu # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 476
(edef Celu (Vec Float) ((X : (Vec Float)) (alpha : Float)))
;; Doing ConcatFromSequence # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/sequence/defs.cc" 608
(edef
  ConcatFromSequence
  (Vec Bool)
  ((input_sequence : (Vec (Vec Integer)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec String)
  ((input_sequence : (Vec (Vec Integer)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Float)
  ((input_sequence : (Vec (Vec Integer)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Integer)
  ((input_sequence : (Vec (Vec Integer)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec Integer)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Bool)
  ((input_sequence : (Vec (Vec Float)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec String)
  ((input_sequence : (Vec (Vec Float)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Float)
  ((input_sequence : (Vec (Vec Float)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Integer)
  ((input_sequence : (Vec (Vec Float)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec Float)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Bool)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec String)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Float)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Integer)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Bool)
  ((input_sequence : (Vec (Vec Bool)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec String)
  ((input_sequence : (Vec (Vec Bool)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Float)
  ((input_sequence : (Vec (Vec Bool)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Integer)
  ((input_sequence : (Vec (Vec Bool)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec Bool)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Bool)
  ((input_sequence : (Vec (Vec String)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec String)
  ((input_sequence : (Vec (Vec String)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Float)
  ((input_sequence : (Vec (Vec String)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec Integer)
  ((input_sequence : (Vec (Vec String)))
   (axis : Integer)
   (new_axis : Integer)))
(edef
  ConcatFromSequence
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec String)))
   (axis : Integer)
   (new_axis : Integer)))
;; Doing SoftmaxCrossEntropyLoss # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 2489
(edef
  SoftmaxCrossEntropyLoss
  (Vec Float)
  ((scores : (Vec Float))
   (labels : (Vec Integer))
   (weights : (Vec Float))
   (ignore_index : Integer)
   (reduction : String)))
;; Doing SplitToSequence # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/sequence/defs.cc" 512
(edef
  SplitToSequence
  (Vec (Vec Integer))
  ((input : (Vec Bool))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Float))
  ((input : (Vec Bool))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec (Tuple Float Float)))
  ((input : (Vec Bool))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Bool))
  ((input : (Vec Bool))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec String))
  ((input : (Vec Bool))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Integer))
  ((input : (Vec String))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Float))
  ((input : (Vec String))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec (Tuple Float Float)))
  ((input : (Vec String))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Bool))
  ((input : (Vec String))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec String))
  ((input : (Vec String))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Integer))
  ((input : (Vec Float))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Float))
  ((input : (Vec Float))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec (Tuple Float Float)))
  ((input : (Vec Float))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Bool))
  ((input : (Vec Float))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec String))
  ((input : (Vec Float))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Integer))
  ((input : (Vec Integer))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Float))
  ((input : (Vec Integer))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec (Tuple Float Float)))
  ((input : (Vec Integer))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Bool))
  ((input : (Vec Integer))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec String))
  ((input : (Vec Integer))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Integer))
  ((input : (Vec (Tuple Float Float)))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Float))
  ((input : (Vec (Tuple Float Float)))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec (Tuple Float Float)))
  ((input : (Vec (Tuple Float Float)))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec Bool))
  ((input : (Vec (Tuple Float Float)))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
(edef
  SplitToSequence
  (Vec (Vec String))
  ((input : (Vec (Tuple Float Float)))
   (split : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)))
;; Doing SequenceErase # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/sequence/defs.cc" 290
(edef
  SequenceErase
  (Vec (Vec Integer))
  ((input_sequence : (Vec (Vec Integer))) (position : (Vec Integer))))
(edef
  SequenceErase
  (Vec (Vec Float))
  ((input_sequence : (Vec (Vec Float))) (position : (Vec Integer))))
(edef
  SequenceErase
  (Vec (Vec (Tuple Float Float)))
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (position : (Vec Integer))))
(edef
  SequenceErase
  (Vec (Vec Bool))
  ((input_sequence : (Vec (Vec Bool))) (position : (Vec Integer))))
(edef
  SequenceErase
  (Vec (Vec String))
  ((input_sequence : (Vec (Vec String))) (position : (Vec Integer))))
;; Doing SVMRegressor # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 870
(edef
  SVMRegressor
  None
  ((X : (Vec Float))
   (coefficients : (Vec Float))
   (kernel_params : (Vec Float))
   (kernel_type : String)
   (n_supports : Integer)
   (one_class : Integer)
   (post_transform : String)
   (rho : (Vec Float))
   (support_vectors : (Vec Float))))
(edef
  SVMRegressor
  None
  ((X : (Vec Integer))
   (coefficients : (Vec Float))
   (kernel_params : (Vec Float))
   (kernel_type : String)
   (n_supports : Integer)
   (one_class : Integer)
   (post_transform : String)
   (rho : (Vec Float))
   (support_vectors : (Vec Float))))
;; Doing SequenceAt # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/sequence/defs.cc" 245
(edef
  SequenceAt
  (Vec Bool)
  ((input_sequence : (Vec (Vec Integer))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec String)
  ((input_sequence : (Vec (Vec Integer))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Float)
  ((input_sequence : (Vec (Vec Integer))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Integer)
  ((input_sequence : (Vec (Vec Integer))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec Integer))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Bool)
  ((input_sequence : (Vec (Vec Float))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec String)
  ((input_sequence : (Vec (Vec Float))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Float)
  ((input_sequence : (Vec (Vec Float))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Integer)
  ((input_sequence : (Vec (Vec Float))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec Float))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Bool)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec String)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Float)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Integer)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Bool)
  ((input_sequence : (Vec (Vec Bool))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec String)
  ((input_sequence : (Vec (Vec Bool))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Float)
  ((input_sequence : (Vec (Vec Bool))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Integer)
  ((input_sequence : (Vec (Vec Bool))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec Bool))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Bool)
  ((input_sequence : (Vec (Vec String))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec String)
  ((input_sequence : (Vec (Vec String))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Float)
  ((input_sequence : (Vec (Vec String))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec Integer)
  ((input_sequence : (Vec (Vec String))) (position : (Vec Integer))))
(edef
  SequenceAt
  (Vec (Tuple Float Float))
  ((input_sequence : (Vec (Vec String))) (position : (Vec Integer))))
;; Doing SequenceInsert # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/sequence/defs.cc" 198
(edef
  SequenceInsert
  (Vec (Vec Integer))
  ((input_sequence : (Vec (Vec Integer)))
   (tensor : (Vec Bool))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Float))
  ((input_sequence : (Vec (Vec Float)))
   (tensor : (Vec Bool))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec (Tuple Float Float)))
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (tensor : (Vec Bool))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Bool))
  ((input_sequence : (Vec (Vec Bool)))
   (tensor : (Vec Bool))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec String))
  ((input_sequence : (Vec (Vec String)))
   (tensor : (Vec Bool))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Integer))
  ((input_sequence : (Vec (Vec Integer)))
   (tensor : (Vec String))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Float))
  ((input_sequence : (Vec (Vec Float)))
   (tensor : (Vec String))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec (Tuple Float Float)))
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (tensor : (Vec String))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Bool))
  ((input_sequence : (Vec (Vec Bool)))
   (tensor : (Vec String))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec String))
  ((input_sequence : (Vec (Vec String)))
   (tensor : (Vec String))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Integer))
  ((input_sequence : (Vec (Vec Integer)))
   (tensor : (Vec Float))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Float))
  ((input_sequence : (Vec (Vec Float)))
   (tensor : (Vec Float))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec (Tuple Float Float)))
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (tensor : (Vec Float))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Bool))
  ((input_sequence : (Vec (Vec Bool)))
   (tensor : (Vec Float))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec String))
  ((input_sequence : (Vec (Vec String)))
   (tensor : (Vec Float))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Integer))
  ((input_sequence : (Vec (Vec Integer)))
   (tensor : (Vec Integer))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Float))
  ((input_sequence : (Vec (Vec Float)))
   (tensor : (Vec Integer))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec (Tuple Float Float)))
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (tensor : (Vec Integer))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Bool))
  ((input_sequence : (Vec (Vec Bool)))
   (tensor : (Vec Integer))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec String))
  ((input_sequence : (Vec (Vec String)))
   (tensor : (Vec Integer))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Integer))
  ((input_sequence : (Vec (Vec Integer)))
   (tensor : (Vec (Tuple Float Float)))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Float))
  ((input_sequence : (Vec (Vec Float)))
   (tensor : (Vec (Tuple Float Float)))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec (Tuple Float Float)))
  ((input_sequence : (Vec (Vec (Tuple Float Float))))
   (tensor : (Vec (Tuple Float Float)))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec Bool))
  ((input_sequence : (Vec (Vec Bool)))
   (tensor : (Vec (Tuple Float Float)))
   (position : (Vec Integer))))
(edef
  SequenceInsert
  (Vec (Vec String))
  ((input_sequence : (Vec (Vec String)))
   (tensor : (Vec (Tuple Float Float)))
   (position : (Vec Integer))))
;; Doing SequenceConstruct # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/sequence/defs.cc" 116
(edef SequenceConstruct (Vec (Vec Integer)) ((inputs : (Vec Bool))))
(edef SequenceConstruct (Vec (Vec Float)) ((inputs : (Vec Bool))))
(edef
  SequenceConstruct
  (Vec (Vec (Tuple Float Float)))
  ((inputs : (Vec Bool))))
(edef SequenceConstruct (Vec (Vec Bool)) ((inputs : (Vec Bool))))
(edef SequenceConstruct (Vec (Vec String)) ((inputs : (Vec Bool))))
(edef SequenceConstruct (Vec (Vec Integer)) ((inputs : (Vec String))))
(edef SequenceConstruct (Vec (Vec Float)) ((inputs : (Vec String))))
(edef
  SequenceConstruct
  (Vec (Vec (Tuple Float Float)))
  ((inputs : (Vec String))))
(edef SequenceConstruct (Vec (Vec Bool)) ((inputs : (Vec String))))
(edef SequenceConstruct (Vec (Vec String)) ((inputs : (Vec String))))
(edef SequenceConstruct (Vec (Vec Integer)) ((inputs : (Vec Float))))
(edef SequenceConstruct (Vec (Vec Float)) ((inputs : (Vec Float))))
(edef
  SequenceConstruct
  (Vec (Vec (Tuple Float Float)))
  ((inputs : (Vec Float))))
(edef SequenceConstruct (Vec (Vec Bool)) ((inputs : (Vec Float))))
(edef SequenceConstruct (Vec (Vec String)) ((inputs : (Vec Float))))
(edef SequenceConstruct (Vec (Vec Integer)) ((inputs : (Vec Integer))))
(edef SequenceConstruct (Vec (Vec Float)) ((inputs : (Vec Integer))))
(edef
  SequenceConstruct
  (Vec (Vec (Tuple Float Float)))
  ((inputs : (Vec Integer))))
(edef SequenceConstruct (Vec (Vec Bool)) ((inputs : (Vec Integer))))
(edef SequenceConstruct (Vec (Vec String)) ((inputs : (Vec Integer))))
(edef
  SequenceConstruct
  (Vec (Vec Integer))
  ((inputs : (Vec (Tuple Float Float)))))
(edef
  SequenceConstruct
  (Vec (Vec Float))
  ((inputs : (Vec (Tuple Float Float)))))
(edef
  SequenceConstruct
  (Vec (Vec (Tuple Float Float)))
  ((inputs : (Vec (Tuple Float Float)))))
(edef
  SequenceConstruct
  (Vec (Vec Bool))
  ((inputs : (Vec (Tuple Float Float)))))
(edef
  SequenceConstruct
  (Vec (Vec String))
  ((inputs : (Vec (Tuple Float Float)))))
;; Doing SequenceEmpty # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/sequence/defs.cc" 52
(edef SequenceEmpty (Vec (Vec Integer)) ((dtype : Integer)))
(edef SequenceEmpty (Vec (Vec Float)) ((dtype : Integer)))
(edef
  SequenceEmpty
  (Vec (Vec (Tuple Float Float)))
  ((dtype : Integer)))
(edef SequenceEmpty (Vec (Vec Bool)) ((dtype : Integer)))
(edef SequenceEmpty (Vec (Vec String)) ((dtype : Integer)))
;; Doing Det # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1783
(edef Det (Vec Float) ((X : (Vec Float))))
;; Doing CumSum # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1700
(edef
  CumSum
  (Vec Float)
  ((x : (Vec Float))
   (axis : (Vec Integer))
   (exclusive : Integer)
   (reverse : Integer)))
(edef
  CumSum
  (Vec Integer)
  ((x : (Vec Integer))
   (axis : (Vec Integer))
   (exclusive : Integer)
   (reverse : Integer)))
;; Doing RoiAlign # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/object_detection/defs.cc" 127
(edef
  RoiAlign
  (Vec Float)
  ((X : (Vec Float))
   (rois : (Vec Float))
   (batch_indices : (Vec Integer))
   (mode : String)
   (output_height : Integer)
   (output_width : Integer)
   (sampling_ratio : Integer)
   (spatial_scale : Float)))
;; Doing NonMaxSuppression # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/object_detection/defs.cc" 208
(edef
  NonMaxSuppression
  None
  ((boxes : None)
   (scores : None)
   (max_output_boxes_per_class : None)
   (iou_threshold : None)
   (score_threshold : None)
   (center_point_box : Integer)))
;; Doing TreeEnsembleRegressor # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 1116
(edef
  TreeEnsembleRegressor
  None
  ((X : (Vec Float))
   (aggregate_function : String)
   (base_values : (Vec Float))
   (n_targets : Integer)
   (nodes_falsenodeids : (Vec Integer))
   (nodes_featureids : (Vec Integer))
   (nodes_hitrates : (Vec Float))
   (nodes_missing_value_tracks_true : (Vec Integer))
   (nodes_modes : (Vec String))
   (nodes_nodeids : (Vec Integer))
   (nodes_treeids : (Vec Integer))
   (nodes_truenodeids : (Vec Integer))
   (nodes_values : (Vec Float))
   (post_transform : String)
   (target_ids : (Vec Integer))
   (target_nodeids : (Vec Integer))
   (target_treeids : (Vec Integer))
   (target_weights : (Vec Float))))
(edef
  TreeEnsembleRegressor
  None
  ((X : (Vec Integer))
   (aggregate_function : String)
   (base_values : (Vec Float))
   (n_targets : Integer)
   (nodes_falsenodeids : (Vec Integer))
   (nodes_featureids : (Vec Integer))
   (nodes_hitrates : (Vec Float))
   (nodes_missing_value_tracks_true : (Vec Integer))
   (nodes_modes : (Vec String))
   (nodes_nodeids : (Vec Integer))
   (nodes_treeids : (Vec Integer))
   (nodes_truenodeids : (Vec Integer))
   (nodes_values : (Vec Float))
   (post_transform : String)
   (target_ids : (Vec Integer))
   (target_nodeids : (Vec Integer))
   (target_treeids : (Vec Integer))
   (target_weights : (Vec Float))))
;; Doing IsInf # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2102
(edef
  IsInf
  (Vec Bool)
  ((X : (Vec Float))
   (detect_negative : Integer)
   (detect_positive : Integer)))
;; Doing QLinearConv # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 964
(edef
  QLinearConv
  (Vec Integer)
  ((x : (Vec Integer))
   (x_scale : None)
   (x_zero_point : (Vec Integer))
   (w : (Vec Integer))
   (w_scale : None)
   (w_zero_point : (Vec Integer))
   (y_scale : None)
   (y_zero_point : (Vec Integer))
   (B : (Vec Integer))
   (auto_pad : String)
   (dilations : (Vec Integer))
   (group : Integer)
   (kernel_shape : (Vec Integer))
   (pads : (Vec Integer))
   (strides : (Vec Integer))))
;; Doing ConvInteger # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1090
(edef
  ConvInteger
  (Vec Integer)
  ((x : (Vec Integer))
   (w : (Vec Integer))
   (x_zero_point : (Vec Integer))
   (w_zero_point : (Vec Integer))
   (auto_pad : String)
   (dilations : (Vec Integer))
   (group : Integer)
   (kernel_shape : (Vec Integer))
   (pads : (Vec Integer))
   (strides : (Vec Integer))))
;; Doing Round # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1729
(edef Round (Vec Float) ((X : (Vec Float))))
;; Doing QLinearMatMul # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1574
(edef
  QLinearMatMul
  (Vec Integer)
  ((a : (Vec Integer))
   (a_scale : None)
   (a_zero_point : (Vec Integer))
   (b : (Vec Integer))
   (b_scale : None)
   (b_zero_point : (Vec Integer))
   (y_scale : None)
   (y_zero_point : (Vec Integer))))
;; Doing Range # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 888
;; has body: <class 'onnx.onnx_operators_ml_pb2.FunctionProto'>
(edef
  Range
  (Vec Float)
  ((start : (Vec Float)) (limit : (Vec Float)) (delta : (Vec Float))))
;; has body: <class 'onnx.onnx_operators_ml_pb2.FunctionProto'>
(edef
  Range
  (Vec Integer)
  ((start : (Vec Integer))
   (limit : (Vec Integer))
   (delta : (Vec Integer))))
;; Doing ThresholdedRelu # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 360
(edef ThresholdedRelu (Vec Float) ((X : (Vec Float)) (alpha : Float)))
;; Doing StringNormalizer # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 2083
(edef
  StringNormalizer
  None
  ((X : None)
   (case_change_action : String)
   (is_case_sensitive : Integer)
   (locale : String)
   (stopwords : (Vec String))))
;; Doing MeanVarianceNormalization # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 2129
;; has body: <class 'onnx.onnx_operators_ml_pb2.FunctionProto'>
(edef
  MeanVarianceNormalization
  (Vec Float)
  ((X : (Vec Float)) (axes : (Vec Integer))))
;; Doing NonZero # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2174
(edef NonZero None ((X : (Vec Bool))))
(edef NonZero None ((X : (Vec String))))
(edef NonZero None ((X : (Vec Float))))
(edef NonZero None ((X : (Vec Integer))))
(edef NonZero None ((X : (Vec (Tuple Float Float)))))
;; Doing Where # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2152
(edef
  Where
  (Vec Bool)
  ((condition : (Vec Bool)) (X : (Vec Bool)) (Y : (Vec Bool))))
(edef
  Where
  (Vec String)
  ((condition : (Vec Bool)) (X : (Vec String)) (Y : (Vec String))))
(edef
  Where
  (Vec Float)
  ((condition : (Vec Bool)) (X : (Vec Float)) (Y : (Vec Float))))
(edef
  Where
  (Vec Integer)
  ((condition : (Vec Bool)) (X : (Vec Integer)) (Y : (Vec Integer))))
(edef
  Where
  (Vec (Tuple Float Float))
  ((condition : (Vec Bool))
   (X : (Vec (Tuple Float Float)))
   (Y : (Vec (Tuple Float Float)))))
;; Doing Shrink # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1774
(edef
  Shrink
  (Vec Float)
  ((input : (Vec Float)) (bias : Float) (lambd : Float)))
(edef
  Shrink
  (Vec Integer)
  ((input : (Vec Integer)) (bias : Float) (lambd : Float)))
;; Doing Erf # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1506
(edef Erf (Vec Float) ((input : (Vec Float))))
(edef Erf (Vec Integer) ((input : (Vec Integer))))
;; Doing Atanh # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1461
(edef Atanh (Vec Float) ((input : (Vec Float))))
;; Doing Acosh # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1439
(edef Acosh (Vec Float) ((input : (Vec Float))))
;; Doing TreeEnsembleClassifier # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 1003
(edef
  TreeEnsembleClassifier
  None
  ((X : (Vec Float))
   (base_values : (Vec Float))
   (class_ids : (Vec Integer))
   (class_nodeids : (Vec Integer))
   (class_treeids : (Vec Integer))
   (class_weights : (Vec Float))
   (classlabels_int64s : (Vec Integer))
   (classlabels_strings : (Vec String))
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
(edef
  TreeEnsembleClassifier
  None
  ((X : (Vec Float))
   (base_values : (Vec Float))
   (class_ids : (Vec Integer))
   (class_nodeids : (Vec Integer))
   (class_treeids : (Vec Integer))
   (class_weights : (Vec Float))
   (classlabels_int64s : (Vec Integer))
   (classlabels_strings : (Vec String))
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
(edef
  TreeEnsembleClassifier
  None
  ((X : (Vec Integer))
   (base_values : (Vec Float))
   (class_ids : (Vec Integer))
   (class_nodeids : (Vec Integer))
   (class_treeids : (Vec Integer))
   (class_weights : (Vec Float))
   (classlabels_int64s : (Vec Integer))
   (classlabels_strings : (Vec String))
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
(edef
  TreeEnsembleClassifier
  None
  ((X : (Vec Integer))
   (base_values : (Vec Float))
   (class_ids : (Vec Integer))
   (class_nodeids : (Vec Integer))
   (class_treeids : (Vec Integer))
   (class_weights : (Vec Float))
   (classlabels_int64s : (Vec Integer))
   (classlabels_strings : (Vec String))
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
;; Doing OneHot # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2044
(edef
  OneHot
  (Vec Bool)
  ((indices : (Vec Float))
   (depth : (Vec Float))
   (values : (Vec Bool))
   (axis : Integer)))
(edef
  OneHot
  (Vec String)
  ((indices : (Vec Float))
   (depth : (Vec Float))
   (values : (Vec String))
   (axis : Integer)))
(edef
  OneHot
  (Vec Float)
  ((indices : (Vec Float))
   (depth : (Vec Float))
   (values : (Vec Float))
   (axis : Integer)))
(edef
  OneHot
  (Vec Integer)
  ((indices : (Vec Float))
   (depth : (Vec Float))
   (values : (Vec Integer))
   (axis : Integer)))
(edef
  OneHot
  (Vec (Tuple Float Float))
  ((indices : (Vec Float))
   (depth : (Vec Float))
   (values : (Vec (Tuple Float Float)))
   (axis : Integer)))
(edef
  OneHot
  (Vec Bool)
  ((indices : (Vec Float))
   (depth : (Vec Integer))
   (values : (Vec Bool))
   (axis : Integer)))
(edef
  OneHot
  (Vec String)
  ((indices : (Vec Float))
   (depth : (Vec Integer))
   (values : (Vec String))
   (axis : Integer)))
(edef
  OneHot
  (Vec Float)
  ((indices : (Vec Float))
   (depth : (Vec Integer))
   (values : (Vec Float))
   (axis : Integer)))
(edef
  OneHot
  (Vec Integer)
  ((indices : (Vec Float))
   (depth : (Vec Integer))
   (values : (Vec Integer))
   (axis : Integer)))
(edef
  OneHot
  (Vec (Tuple Float Float))
  ((indices : (Vec Float))
   (depth : (Vec Integer))
   (values : (Vec (Tuple Float Float)))
   (axis : Integer)))
(edef
  OneHot
  (Vec Bool)
  ((indices : (Vec Integer))
   (depth : (Vec Float))
   (values : (Vec Bool))
   (axis : Integer)))
(edef
  OneHot
  (Vec String)
  ((indices : (Vec Integer))
   (depth : (Vec Float))
   (values : (Vec String))
   (axis : Integer)))
(edef
  OneHot
  (Vec Float)
  ((indices : (Vec Integer))
   (depth : (Vec Float))
   (values : (Vec Float))
   (axis : Integer)))
(edef
  OneHot
  (Vec Integer)
  ((indices : (Vec Integer))
   (depth : (Vec Float))
   (values : (Vec Integer))
   (axis : Integer)))
(edef
  OneHot
  (Vec (Tuple Float Float))
  ((indices : (Vec Integer))
   (depth : (Vec Float))
   (values : (Vec (Tuple Float Float)))
   (axis : Integer)))
(edef
  OneHot
  (Vec Bool)
  ((indices : (Vec Integer))
   (depth : (Vec Integer))
   (values : (Vec Bool))
   (axis : Integer)))
(edef
  OneHot
  (Vec String)
  ((indices : (Vec Integer))
   (depth : (Vec Integer))
   (values : (Vec String))
   (axis : Integer)))
(edef
  OneHot
  (Vec Float)
  ((indices : (Vec Integer))
   (depth : (Vec Integer))
   (values : (Vec Float))
   (axis : Integer)))
(edef
  OneHot
  (Vec Integer)
  ((indices : (Vec Integer))
   (depth : (Vec Integer))
   (values : (Vec Integer))
   (axis : Integer)))
(edef
  OneHot
  (Vec (Tuple Float Float))
  ((indices : (Vec Integer))
   (depth : (Vec Integer))
   (values : (Vec (Tuple Float Float)))
   (axis : Integer)))
;; Doing TfIdfVectorizer # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 2010
(edef
  TfIdfVectorizer
  (Vec Float)
  ((X : (Vec String))
   (max_gram_length : Integer)
   (max_skip_count : Integer)
   (min_gram_length : Integer)
   (mode : String)
   (ngram_counts : (Vec Integer))
   (ngram_indexes : (Vec Integer))
   (pool_int64s : (Vec Integer))
   (pool_strings : (Vec String))
   (weights : (Vec Float))))
(edef
  TfIdfVectorizer
  (Vec Float)
  ((X : (Vec Integer))
   (max_gram_length : Integer)
   (max_skip_count : Integer)
   (min_gram_length : Integer)
   (mode : String)
   (ngram_counts : (Vec Integer))
   (ngram_indexes : (Vec Integer))
   (pool_int64s : (Vec Integer))
   (pool_strings : (Vec String))
   (weights : (Vec Float))))
;; Doing MaxUnpool # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 539
(edef
  MaxUnpool
  (Vec Float)
  ((X : (Vec Float))
   (I : (Vec Integer))
   (output_shape : (Vec Integer))
   (kernel_shape : (Vec Integer))
   (pads : (Vec Integer))
   (strides : (Vec Integer))))
;; Doing EyeLike # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 370
(edef
  EyeLike
  (Vec Float)
  ((input : (Vec Float)) (dtype : Integer) (k : Integer)))
(edef
  EyeLike
  (Vec Bool)
  ((input : (Vec Float)) (dtype : Integer) (k : Integer)))
(edef
  EyeLike
  (Vec Integer)
  ((input : (Vec Float)) (dtype : Integer) (k : Integer)))
(edef
  EyeLike
  (Vec Float)
  ((input : (Vec Bool)) (dtype : Integer) (k : Integer)))
(edef
  EyeLike
  (Vec Bool)
  ((input : (Vec Bool)) (dtype : Integer) (k : Integer)))
(edef
  EyeLike
  (Vec Integer)
  ((input : (Vec Bool)) (dtype : Integer) (k : Integer)))
(edef
  EyeLike
  (Vec Float)
  ((input : (Vec Integer)) (dtype : Integer) (k : Integer)))
(edef
  EyeLike
  (Vec Bool)
  ((input : (Vec Integer)) (dtype : Integer) (k : Integer)))
(edef
  EyeLike
  (Vec Integer)
  ((input : (Vec Integer)) (dtype : Integer) (k : Integer)))
;; Doing ConstantOfShape # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 286
(edef
  ConstantOfShape
  (Vec Float)
  ((input : (Vec Integer)) (value : (Vec Float))))
(edef
  ConstantOfShape
  (Vec Bool)
  ((input : (Vec Integer)) (value : (Vec Float))))
(edef
  ConstantOfShape
  (Vec Integer)
  ((input : (Vec Integer)) (value : (Vec Float))))
;; Doing Expand # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1351
(edef Expand (Vec Bool) ((input : (Vec Bool)) (shape : None)))
(edef Expand (Vec String) ((input : (Vec String)) (shape : None)))
(edef Expand (Vec Float) ((input : (Vec Float)) (shape : None)))
(edef Expand (Vec Integer) ((input : (Vec Integer)) (shape : None)))
(edef
  Expand
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float))) (shape : None)))
;; Doing Multinomial # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 664
(edef
  Multinomial
  (Vec Integer)
  ((input : (Vec Float))
   (dtype : Integer)
   (sample_size : Integer)
   (seed : Float)))
;; Doing Atan # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1292
(edef Atan (Vec Float) ((input : (Vec Float))))
;; Doing LpNormalization # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1659
(edef
  LpNormalization
  (Vec Float)
  ((input : (Vec Float)) (axis : Integer) (p : Integer)))
;; Doing Ceil # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 282
(edef Ceil (Vec Float) ((X : (Vec Float))))
;; Doing RandomUniformLike # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 534
(edef
  RandomUniformLike
  (Vec Float)
  ((input : (Vec Bool))
   (dtype : Integer)
   (high : Float)
   (low : Float)
   (seed : Float)))
(edef
  RandomUniformLike
  (Vec Float)
  ((input : (Vec String))
   (dtype : Integer)
   (high : Float)
   (low : Float)
   (seed : Float)))
(edef
  RandomUniformLike
  (Vec Float)
  ((input : (Vec Float))
   (dtype : Integer)
   (high : Float)
   (low : Float)
   (seed : Float)))
(edef
  RandomUniformLike
  (Vec Float)
  ((input : (Vec Integer))
   (dtype : Integer)
   (high : Float)
   (low : Float)
   (seed : Float)))
(edef
  RandomUniformLike
  (Vec Float)
  ((input : (Vec (Tuple Float Float)))
   (dtype : Integer)
   (high : Float)
   (low : Float)
   (seed : Float)))
;; Doing LSTM # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/rnn/defs.cc" 503
(edef
  LSTM
  (Vec Float)
  ((X : (Vec Float))
   (W : (Vec Float))
   (R : (Vec Float))
   (B : (Vec Float))
   (sequence_lens : (Vec Integer))
   (initial_h : (Vec Float))
   (initial_c : (Vec Float))
   (P : (Vec Float))
   (activation_alpha : (Vec Float))
   (activation_beta : (Vec Float))
   (activations : (Vec String))
   (clip : Float)
   (direction : String)
   (hidden_size : Integer)
   (input_forget : Integer)))
;; Doing LogSoftmax # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 789
(edef LogSoftmax (Vec Float) ((input : (Vec Float)) (axis : Integer)))
;; Doing MatMul # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1035
(edef MatMul (Vec Float) ((A : (Vec Float)) (B : (Vec Float))))
(edef MatMul (Vec Integer) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing BitShift # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 204
(edef
  BitShift
  (Vec Integer)
  ((X : (Vec Integer)) (Y : (Vec Integer)) (direction : String)))
;; Doing Sinh # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1373
(edef Sinh (Vec Float) ((input : (Vec Float))))
;; Doing Acos # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1270
(edef Acos (Vec Float) ((input : (Vec Float))))
;; Doing Identity # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1825
(edef Identity (Vec Bool) ((input : (Vec Bool))))
(edef Identity (Vec String) ((input : (Vec String))))
(edef Identity (Vec Float) ((input : (Vec Float))))
(edef Identity (Vec Integer) ((input : (Vec Integer))))
(edef
  Identity
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float)))))
;; Doing Pow # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 588
(edef Pow (Vec Float) ((X : (Vec Float)) (Y : (Vec Float))))
(edef Pow (Vec Float) ((X : (Vec Float)) (Y : (Vec Integer))))
(edef Pow (Vec Integer) ((X : (Vec Integer)) (Y : (Vec Float))))
(edef Pow (Vec Integer) ((X : (Vec Integer)) (Y : (Vec Integer))))
;; Doing LinearClassifier # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 572
(edef
  LinearClassifier
  None
  ((X : (Vec Float))
   (classlabels_ints : (Vec Integer))
   (classlabels_strings : (Vec String))
   (coefficients : (Vec Float))
   (intercepts : (Vec Float))
   (multi_class : Integer)
   (post_transform : String)))
(edef
  LinearClassifier
  None
  ((X : (Vec Float))
   (classlabels_ints : (Vec Integer))
   (classlabels_strings : (Vec String))
   (coefficients : (Vec Float))
   (intercepts : (Vec Float))
   (multi_class : Integer)
   (post_transform : String)))
(edef
  LinearClassifier
  None
  ((X : (Vec Integer))
   (classlabels_ints : (Vec Integer))
   (classlabels_strings : (Vec String))
   (coefficients : (Vec Float))
   (intercepts : (Vec Float))
   (multi_class : Integer)
   (post_transform : String)))
(edef
  LinearClassifier
  None
  ((X : (Vec Integer))
   (classlabels_ints : (Vec Integer))
   (classlabels_strings : (Vec String))
   (coefficients : (Vec Float))
   (intercepts : (Vec Float))
   (multi_class : Integer)
   (post_transform : String)))
;; Doing Mod # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 171
(edef
  Mod
  (Vec Float)
  ((A : (Vec Float)) (B : (Vec Float)) (fmod : Integer)))
(edef
  Mod
  (Vec Integer)
  ((A : (Vec Integer)) (B : (Vec Integer)) (fmod : Integer)))
;; Doing Shape # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 264
(edef Shape (Vec Integer) ((data : (Vec Bool))))
(edef Shape (Vec Integer) ((data : (Vec String))))
(edef Shape (Vec Integer) ((data : (Vec Float))))
(edef Shape (Vec Integer) ((data : (Vec Integer))))
(edef Shape (Vec Integer) ((data : (Vec (Tuple Float Float)))))
;; Doing If # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/controlflow/defs.cc" 423
(edef
  If
  (Vec Bool)
  ((cond : (Vec Bool))
   (else_branch : (Lam None(None)))
   (then_branch : (Lam None(None)))))
(edef
  If
  (Vec String)
  ((cond : (Vec Bool))
   (else_branch : (Lam None(None)))
   (then_branch : (Lam None(None)))))
(edef
  If
  (Vec Float)
  ((cond : (Vec Bool))
   (else_branch : (Lam None(None)))
   (then_branch : (Lam None(None)))))
(edef
  If
  (Vec Integer)
  ((cond : (Vec Bool))
   (else_branch : (Lam None(None)))
   (then_branch : (Lam None(None)))))
(edef
  If
  (Vec (Tuple Float Float))
  ((cond : (Vec Bool))
   (else_branch : (Lam None(None)))
   (then_branch : (Lam None(None)))))
;; Doing Softplus # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 836
(edef Softplus (Vec Float) ((X : (Vec Float))))
;; Doing Normalizer # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 657
(edef Normalizer None ((X : (Vec Float)) (norm : String)))
(edef Normalizer None ((X : (Vec Integer)) (norm : String)))
;; Doing Hardmax # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 796
(edef Hardmax (Vec Float) ((input : (Vec Float)) (axis : Integer)))
;; Doing HardSigmoid # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 661
(edef
  HardSigmoid
  (Vec Float)
  ((X : (Vec Float)) (alpha : Float) (beta : Float)))
;; Doing LpPool # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 605
(edef
  LpPool
  (Vec Float)
  ((X : (Vec Float))
   (auto_pad : String)
   (kernel_shape : (Vec Integer))
   (p : Integer)
   (pads : (Vec Integer))
   (strides : (Vec Integer))))
;; Doing SVMClassifier # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 811
(edef
  SVMClassifier
  None
  ((X : (Vec Float))
   (classlabels_ints : (Vec Integer))
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
  None
  ((X : (Vec Float))
   (classlabels_ints : (Vec Integer))
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
  None
  ((X : (Vec Integer))
   (classlabels_ints : (Vec Integer))
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
  None
  ((X : (Vec Integer))
   (classlabels_ints : (Vec Integer))
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
;; Doing Min # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 724
(edef Min (Vec Float) ((data_0 : (Vec Float))))
(edef Min (Vec Integer) ((data_0 : (Vec Integer))))
;; Doing GatherElements # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1283
(edef
  GatherElements
  (Vec Bool)
  ((data : (Vec Bool)) (indices : (Vec Integer)) (axis : Integer)))
(edef
  GatherElements
  (Vec String)
  ((data : (Vec String)) (indices : (Vec Integer)) (axis : Integer)))
(edef
  GatherElements
  (Vec Float)
  ((data : (Vec Float)) (indices : (Vec Integer)) (axis : Integer)))
(edef
  GatherElements
  (Vec Integer)
  ((data : (Vec Integer)) (indices : (Vec Integer)) (axis : Integer)))
(edef
  GatherElements
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float)))
   (indices : (Vec Integer))
   (axis : Integer)))
;; Doing QuantizeLinear # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/quantization/defs.cc" 55
(edef
  QuantizeLinear
  (Vec Integer)
  ((x : (Vec Float)) (y_scale : None) (y_zero_point : (Vec Integer))))
(edef
  QuantizeLinear
  (Vec Integer)
  ((x : (Vec Integer)) (y_scale : None) (y_zero_point : (Vec Integer))))
;; Doing Sum # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 734
(edef Sum (Vec Float) ((data_0 : (Vec Float))))
;; Doing MaxPool # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 363
(edef
  MaxPool
  (Vec Integer)
  ((X : (Vec Float))
   (auto_pad : String)
   (ceil_mode : Integer)
   (dilations : (Vec Integer))
   (kernel_shape : (Vec Integer))
   (pads : (Vec Integer))
   (storage_order : Integer)
   (strides : (Vec Integer))))
(edef
  MaxPool
  (Vec Integer)
  ((X : (Vec Integer))
   (auto_pad : String)
   (ceil_mode : Integer)
   (dilations : (Vec Integer))
   (kernel_shape : (Vec Integer))
   (pads : (Vec Integer))
   (storage_order : Integer)
   (strides : (Vec Integer))))
;; Doing Transpose # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 794
(edef
  Transpose
  (Vec Bool)
  ((data : (Vec Bool)) (perm : (Vec Integer))))
(edef
  Transpose
  (Vec String)
  ((data : (Vec String)) (perm : (Vec Integer))))
(edef
  Transpose
  (Vec Float)
  ((data : (Vec Float)) (perm : (Vec Integer))))
(edef
  Transpose
  (Vec Integer)
  ((data : (Vec Integer)) (perm : (Vec Integer))))
(edef
  Transpose
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float))) (perm : (Vec Integer))))
;; Doing Resize # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1812
(edef
  Resize
  (Vec Bool)
  ((X : (Vec Bool))
   (roi : (Vec Float))
   (scales : None)
   (sizes : None)
   (coordinate_transformation_mode : String)
   (cubic_coeff_a : Float)
   (exclude_outside : Integer)
   (extrapolation_value : Float)
   (mode : String)
   (nearest_mode : String)))
(edef
  Resize
  (Vec String)
  ((X : (Vec String))
   (roi : (Vec Float))
   (scales : None)
   (sizes : None)
   (coordinate_transformation_mode : String)
   (cubic_coeff_a : Float)
   (exclude_outside : Integer)
   (extrapolation_value : Float)
   (mode : String)
   (nearest_mode : String)))
(edef
  Resize
  (Vec Float)
  ((X : (Vec Float))
   (roi : (Vec Float))
   (scales : None)
   (sizes : None)
   (coordinate_transformation_mode : String)
   (cubic_coeff_a : Float)
   (exclude_outside : Integer)
   (extrapolation_value : Float)
   (mode : String)
   (nearest_mode : String)))
(edef
  Resize
  (Vec Integer)
  ((X : (Vec Integer))
   (roi : (Vec Float))
   (scales : None)
   (sizes : None)
   (coordinate_transformation_mode : String)
   (cubic_coeff_a : Float)
   (exclude_outside : Integer)
   (extrapolation_value : Float)
   (mode : String)
   (nearest_mode : String)))
(edef
  Resize
  (Vec (Tuple Float Float))
  ((X : (Vec (Tuple Float Float)))
   (roi : (Vec Float))
   (scales : None)
   (sizes : None)
   (coordinate_transformation_mode : String)
   (cubic_coeff_a : Float)
   (exclude_outside : Integer)
   (extrapolation_value : Float)
   (mode : String)
   (nearest_mode : String)))
;; Doing Greater # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 102
(edef Greater (Vec Bool) ((A : (Vec Float)) (B : (Vec Float))))
(edef Greater (Vec Bool) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing ScatterND # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 973
(edef
  ScatterND
  (Vec Bool)
  ((data : (Vec Bool)) (indices : None) (updates : (Vec Bool))))
(edef
  ScatterND
  (Vec String)
  ((data : (Vec String)) (indices : None) (updates : (Vec String))))
(edef
  ScatterND
  (Vec Float)
  ((data : (Vec Float)) (indices : None) (updates : (Vec Float))))
(edef
  ScatterND
  (Vec Integer)
  ((data : (Vec Integer)) (indices : None) (updates : (Vec Integer))))
(edef
  ScatterND
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float)))
   (indices : None)
   (updates : (Vec (Tuple Float Float)))))
;; Doing GlobalLpPool # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1492
(edef GlobalLpPool (Vec Float) ((X : (Vec Float)) (p : Integer)))
;; Doing Gemm # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 935
(edef
  Gemm
  (Vec Float)
  ((A : (Vec Float))
   (B : (Vec Float))
   (C : (Vec Float))
   (alpha : Float)
   (beta : Float)
   (transA : Integer)
   (transB : Integer)))
(edef
  Gemm
  (Vec Integer)
  ((A : (Vec Integer))
   (B : (Vec Integer))
   (C : (Vec Integer))
   (alpha : Float)
   (beta : Float)
   (transA : Integer)
   (transB : Integer)))
;; Doing ZipMap # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 1166
(edef
  ZipMap
  (Vec (Vec (Tuple String Float)))
  ((X : None)
   (classlabels_int64s : (Vec Integer))
   (classlabels_strings : (Vec String))))
(edef
  ZipMap
  (Vec (Vec (Tuple Integer Float)))
  ((X : None)
   (classlabels_int64s : (Vec Integer))
   (classlabels_strings : (Vec String))))
;; Doing InstanceNormalization # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1630
(edef
  InstanceNormalization
  (Vec Float)
  ((input : (Vec Float))
   (scale : (Vec Float))
   (B : (Vec Float))
   (epsilon : Float)))
;; Doing AveragePool # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 327
(edef
  AveragePool
  (Vec Float)
  ((X : (Vec Float))
   (auto_pad : String)
   (ceil_mode : Integer)
   (count_include_pad : Integer)
   (kernel_shape : (Vec Integer))
   (pads : (Vec Integer))
   (strides : (Vec Integer))))
;; Doing MatMulInteger # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1634
(edef
  MatMulInteger
  (Vec Integer)
  ((A : (Vec Integer))
   (B : (Vec Integer))
   (a_zero_point : (Vec Integer))
   (b_zero_point : (Vec Integer))))
;; Doing Sign # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1484
(edef Sign (Vec Float) ((input : (Vec Float))))
(edef Sign (Vec Integer) ((input : (Vec Integer))))
;; Doing Clip # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 777
(edef
  Clip
  (Vec Float)
  ((input : (Vec Float)) (min : (Vec Float)) (max : (Vec Float))))
(edef
  Clip
  (Vec Integer)
  ((input : (Vec Integer)) (min : (Vec Integer)) (max : (Vec Integer))))
;; Doing Einsum # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 2318
(edef Einsum (Vec Float) ((Inputs : (Vec Float)) (equation : String)))
(edef
  Einsum
  (Vec Integer)
  ((Inputs : (Vec Integer)) (equation : String)))
;; Doing DequantizeLinear # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/quantization/defs.cc" 103
(edef
  DequantizeLinear
  None
  ((x : (Vec Integer)) (x_scale : None) (x_zero_point : (Vec Integer))))
;; Doing LRN # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1879
(edef
  LRN
  (Vec Float)
  ((X : (Vec Float))
   (alpha : Float)
   (beta : Float)
   (bias : Float)
   (size : Integer)))
;; Doing Gradient # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/training/defs.cc" 197
(edef
  Gradient
  (Vec Float)
  ((Inputs : (Vec Bool))
   (xs : (Vec String))
   (y : String)
   (zs : (Vec String))))
(edef
  Gradient
  (Vec Float)
  ((Inputs : (Vec String))
   (xs : (Vec String))
   (y : String)
   (zs : (Vec String))))
(edef
  Gradient
  (Vec Float)
  ((Inputs : (Vec Float))
   (xs : (Vec String))
   (y : String)
   (zs : (Vec String))))
(edef
  Gradient
  (Vec Float)
  ((Inputs : (Vec Integer))
   (xs : (Vec String))
   (y : String)
   (zs : (Vec String))))
(edef
  Gradient
  (Vec Float)
  ((Inputs : (Vec (Tuple Float Float)))
   (xs : (Vec String))
   (y : String)
   (zs : (Vec String))))
;; Doing Equal # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 141
(edef Equal (Vec Bool) ((A : (Vec Float)) (B : (Vec Float))))
(edef Equal (Vec Bool) ((A : (Vec Bool)) (B : (Vec Bool))))
(edef Equal (Vec Bool) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing Elu # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 413
(edef Elu (Vec Float) ((X : (Vec Float)) (alpha : Float)))
;; Doing Sin # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1182
(edef Sin (Vec Float) ((input : (Vec Float))))
;; Doing Pad # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2790
(edef
  Pad
  (Vec Float)
  ((data : (Vec Float))
   (pads : None)
   (constant_value : (Vec Float))
   (mode : String)))
(edef
  Pad
  (Vec Integer)
  ((data : (Vec Integer))
   (pads : None)
   (constant_value : (Vec Integer))
   (mode : String)))
;; Doing Less # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 116
(edef Less (Vec Bool) ((A : (Vec Float)) (B : (Vec Float))))
(edef Less (Vec Bool) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing Scan # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/controlflow/defs.cc" 811
(edef
  Scan
  (Vec Bool)
  ((initial_state_and_scan_inputs : (Vec Bool))
   (body : (Lam None(None)))
   (num_scan_inputs : Integer)
   (scan_input_axes : (Vec Integer))
   (scan_input_directions : (Vec Integer))
   (scan_output_axes : (Vec Integer))
   (scan_output_directions : (Vec Integer))))
(edef
  Scan
  (Vec String)
  ((initial_state_and_scan_inputs : (Vec String))
   (body : (Lam None(None)))
   (num_scan_inputs : Integer)
   (scan_input_axes : (Vec Integer))
   (scan_input_directions : (Vec Integer))
   (scan_output_axes : (Vec Integer))
   (scan_output_directions : (Vec Integer))))
(edef
  Scan
  (Vec Float)
  ((initial_state_and_scan_inputs : (Vec Float))
   (body : (Lam None(None)))
   (num_scan_inputs : Integer)
   (scan_input_axes : (Vec Integer))
   (scan_input_directions : (Vec Integer))
   (scan_output_axes : (Vec Integer))
   (scan_output_directions : (Vec Integer))))
(edef
  Scan
  (Vec Integer)
  ((initial_state_and_scan_inputs : (Vec Integer))
   (body : (Lam None(None)))
   (num_scan_inputs : Integer)
   (scan_input_axes : (Vec Integer))
   (scan_input_directions : (Vec Integer))
   (scan_output_axes : (Vec Integer))
   (scan_output_directions : (Vec Integer))))
(edef
  Scan
  (Vec (Tuple Float Float))
  ((initial_state_and_scan_inputs : (Vec (Tuple Float Float)))
   (body : (Lam None(None)))
   (num_scan_inputs : Integer)
   (scan_input_axes : (Vec Integer))
   (scan_input_directions : (Vec Integer))
   (scan_output_axes : (Vec Integer))
   (scan_output_directions : (Vec Integer))))
;; Doing Concat # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 373
(edef Concat (Vec Bool) ((inputs : (Vec Bool)) (axis : Integer)))
(edef Concat (Vec String) ((inputs : (Vec String)) (axis : Integer)))
(edef Concat (Vec Float) ((inputs : (Vec Float)) (axis : Integer)))
(edef Concat (Vec Integer) ((inputs : (Vec Integer)) (axis : Integer)))
(edef
  Concat
  (Vec (Tuple Float Float))
  ((inputs : (Vec (Tuple Float Float))) (axis : Integer)))
;; Doing And # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 60
(edef And (Vec Bool) ((A : (Vec Bool)) (B : (Vec Bool))))
;; Doing GatherND # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2628
(edef
  GatherND
  (Vec Bool)
  ((data : (Vec Bool)) (indices : None) (batch_dims : Integer)))
(edef
  GatherND
  (Vec String)
  ((data : (Vec String)) (indices : None) (batch_dims : Integer)))
(edef
  GatherND
  (Vec Float)
  ((data : (Vec Float)) (indices : None) (batch_dims : Integer)))
(edef
  GatherND
  (Vec Integer)
  ((data : (Vec Integer)) (indices : None) (batch_dims : Integer)))
(edef
  GatherND
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float)))
   (indices : None)
   (batch_dims : Integer)))
;; Doing Relu # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 320
(edef Relu (Vec Float) ((X : (Vec Float))))
;; Doing Conv # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 793
(edef
  Conv
  (Vec Float)
  ((X : (Vec Float))
   (W : (Vec Float))
   (B : (Vec Float))
   (auto_pad : String)
   (dilations : (Vec Integer))
   (group : Integer)
   (kernel_shape : (Vec Integer))
   (pads : (Vec Integer))
   (strides : (Vec Integer))))
;; Doing Size # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 289
(edef Size (Vec Integer) ((data : (Vec Bool))))
(edef Size (Vec Integer) ((data : (Vec String))))
(edef Size (Vec Integer) ((data : (Vec Float))))
(edef Size (Vec Integer) ((data : (Vec Integer))))
(edef Size (Vec Integer) ((data : (Vec (Tuple Float Float)))))
;; Doing MaxRoiPool # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 699
(edef
  MaxRoiPool
  (Vec Float)
  ((X : (Vec Float))
   (rois : (Vec Float))
   (pooled_shape : (Vec Integer))
   (spatial_scale : Float)))
;; Doing Add # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 124
(edef Add (Vec Float) ((A : (Vec Float)) (B : (Vec Float))))
(edef Add (Vec Integer) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing ArgMax # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 238
(edef
  ArgMax
  None
  ((data : (Vec Float))
   (axis : Integer)
   (keepdims : Integer)
   (select_last_index : Integer)))
(edef
  ArgMax
  None
  ((data : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)
   (select_last_index : Integer)))
;; Doing IsNaN # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2066
(edef IsNaN (Vec Bool) ((X : (Vec Float))))
;; Doing Div # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 181
(edef Div (Vec Float) ((A : (Vec Float)) (B : (Vec Float))))
(edef Div (Vec Integer) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing Tan # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1226
(edef Tan (Vec Float) ((input : (Vec Float))))
;; Doing Cast # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 88
(edef Cast (Vec String) ((input : (Vec String)) (to : Integer)))
(edef Cast (Vec Float) ((input : (Vec String)) (to : Integer)))
(edef Cast (Vec Bool) ((input : (Vec String)) (to : Integer)))
(edef Cast (Vec Integer) ((input : (Vec String)) (to : Integer)))
(edef Cast (Vec String) ((input : (Vec Float)) (to : Integer)))
(edef Cast (Vec Float) ((input : (Vec Float)) (to : Integer)))
(edef Cast (Vec Bool) ((input : (Vec Float)) (to : Integer)))
(edef Cast (Vec Integer) ((input : (Vec Float)) (to : Integer)))
(edef Cast (Vec String) ((input : (Vec Bool)) (to : Integer)))
(edef Cast (Vec Float) ((input : (Vec Bool)) (to : Integer)))
(edef Cast (Vec Bool) ((input : (Vec Bool)) (to : Integer)))
(edef Cast (Vec Integer) ((input : (Vec Bool)) (to : Integer)))
(edef Cast (Vec String) ((input : (Vec Integer)) (to : Integer)))
(edef Cast (Vec Float) ((input : (Vec Integer)) (to : Integer)))
(edef Cast (Vec Bool) ((input : (Vec Integer)) (to : Integer)))
(edef Cast (Vec Integer) ((input : (Vec Integer)) (to : Integer)))
;; Doing ReduceSum # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 115
(edef
  ReduceSum
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceSum
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing ArgMin # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 243
(edef
  ArgMin
  None
  ((data : (Vec Float))
   (axis : Integer)
   (keepdims : Integer)
   (select_last_index : Integer)))
(edef
  ArgMin
  None
  ((data : (Vec Integer))
   (axis : Integer)
   (keepdims : Integer)
   (select_last_index : Integer)))
;; Doing DepthToSpace # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1590
(edef
  DepthToSpace
  (Vec Bool)
  ((input : (Vec Bool)) (blocksize : Integer) (mode : String)))
(edef
  DepthToSpace
  (Vec String)
  ((input : (Vec String)) (blocksize : Integer) (mode : String)))
(edef
  DepthToSpace
  (Vec Float)
  ((input : (Vec Float)) (blocksize : Integer) (mode : String)))
(edef
  DepthToSpace
  (Vec Integer)
  ((input : (Vec Integer)) (blocksize : Integer) (mode : String)))
(edef
  DepthToSpace
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float)))
   (blocksize : Integer)
   (mode : String)))
;; Doing OneHotEncoder # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 703
(edef
  OneHotEncoder
  None
  ((X : (Vec String))
   (cats_int64s : (Vec Integer))
   (cats_strings : (Vec String))
   (zeros : Integer)))
(edef
  OneHotEncoder
  None
  ((X : (Vec Float))
   (cats_int64s : (Vec Integer))
   (cats_strings : (Vec String))
   (zeros : Integer)))
(edef
  OneHotEncoder
  None
  ((X : (Vec Integer))
   (cats_int64s : (Vec Integer))
   (cats_strings : (Vec String))
   (zeros : Integer)))
;; Doing Compress # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1888
(edef
  Compress
  (Vec Bool)
  ((input : (Vec Bool)) (condition : (Vec Bool)) (axis : Integer)))
(edef
  Compress
  (Vec String)
  ((input : (Vec String)) (condition : (Vec Bool)) (axis : Integer)))
(edef
  Compress
  (Vec Float)
  ((input : (Vec Float)) (condition : (Vec Bool)) (axis : Integer)))
(edef
  Compress
  (Vec Integer)
  ((input : (Vec Integer)) (condition : (Vec Bool)) (axis : Integer)))
(edef
  Compress
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float)))
   (condition : (Vec Bool))
   (axis : Integer)))
;; Doing ConvTranspose # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1364
(edef
  ConvTranspose
  (Vec Float)
  ((X : (Vec Float))
   (W : (Vec Float))
   (B : (Vec Float))
   (auto_pad : String)
   (dilations : (Vec Integer))
   (group : Integer)
   (kernel_shape : (Vec Integer))
   (output_padding : (Vec Integer))
   (output_shape : (Vec Integer))
   (pads : (Vec Integer))
   (strides : (Vec Integer))))
;; Doing TopK # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1160
(edef
  TopK
  (Vec Integer)
  ((X : (Vec Float))
   (K : None)
   (axis : Integer)
   (largest : Integer)
   (sorted : Integer)))
(edef
  TopK
  (Vec Integer)
  ((X : (Vec Integer))
   (K : None)
   (axis : Integer)
   (largest : Integer)
   (sorted : Integer)))
;; Doing Gather # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1189
(edef
  Gather
  (Vec Bool)
  ((data : (Vec Bool)) (indices : (Vec Integer)) (axis : Integer)))
(edef
  Gather
  (Vec String)
  ((data : (Vec String)) (indices : (Vec Integer)) (axis : Integer)))
(edef
  Gather
  (Vec Float)
  ((data : (Vec Float)) (indices : (Vec Integer)) (axis : Integer)))
(edef
  Gather
  (Vec Integer)
  ((data : (Vec Integer)) (indices : (Vec Integer)) (axis : Integer)))
(edef
  Gather
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float)))
   (indices : (Vec Integer))
   (axis : Integer)))
;; Doing ReverseSequence # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2254
(edef
  ReverseSequence
  (Vec Bool)
  ((input : (Vec Bool))
   (sequence_lens : None)
   (batch_axis : Integer)
   (time_axis : Integer)))
(edef
  ReverseSequence
  (Vec String)
  ((input : (Vec String))
   (sequence_lens : None)
   (batch_axis : Integer)
   (time_axis : Integer)))
(edef
  ReverseSequence
  (Vec Float)
  ((input : (Vec Float))
   (sequence_lens : None)
   (batch_axis : Integer)
   (time_axis : Integer)))
(edef
  ReverseSequence
  (Vec Integer)
  ((input : (Vec Integer))
   (sequence_lens : None)
   (batch_axis : Integer)
   (time_axis : Integer)))
(edef
  ReverseSequence
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float)))
   (sequence_lens : None)
   (batch_axis : Integer)
   (time_axis : Integer)))
;; Doing Xor # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 88
(edef Xor (Vec Bool) ((A : (Vec Bool)) (B : (Vec Bool))))
;; Doing LabelEncoder # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 459
(edef
  LabelEncoder
  (Vec String)
  ((X : (Vec String))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
(edef
  LabelEncoder
  (Vec Float)
  ((X : (Vec String))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
(edef
  LabelEncoder
  (Vec Integer)
  ((X : (Vec String))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
(edef
  LabelEncoder
  (Vec String)
  ((X : (Vec Float))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
(edef
  LabelEncoder
  (Vec Float)
  ((X : (Vec Float))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
(edef
  LabelEncoder
  (Vec Integer)
  ((X : (Vec Float))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
(edef
  LabelEncoder
  (Vec String)
  ((X : (Vec Integer))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
(edef
  LabelEncoder
  (Vec Float)
  ((X : (Vec Integer))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
(edef
  LabelEncoder
  (Vec Integer)
  ((X : (Vec Integer))
   (default_float : Float)
   (default_int64 : Integer)
   (default_string : String)
   (keys_floats : (Vec Float))
   (keys_int64s : (Vec Integer))
   (keys_strings : (Vec String))
   (values_floats : (Vec Float))
   (values_int64s : (Vec Integer))
   (values_strings : (Vec String))))
;; Doing Dropout # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1744
(edef
  Dropout
  (Vec Bool)
  ((data : (Vec Float))
   (ratio : (Vec Float))
   (training_mode : (Vec Bool))
   (seed : Integer)))
;; Doing Unique # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 2453
(edef
  Unique
  None
  ((X : (Vec Bool)) (axis : Integer) (sorted : Integer)))
(edef
  Unique
  None
  ((X : (Vec String)) (axis : Integer) (sorted : Integer)))
(edef
  Unique
  None
  ((X : (Vec Float)) (axis : Integer) (sorted : Integer)))
(edef
  Unique
  None
  ((X : (Vec Integer)) (axis : Integer) (sorted : Integer)))
(edef
  Unique
  None
  ((X : (Vec (Tuple Float Float))) (axis : Integer) (sorted : Integer)))
;; Doing Max # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 714
(edef Max (Vec Float) ((data_0 : (Vec Float))))
(edef Max (Vec Integer) ((data_0 : (Vec Integer))))
;; Doing LeakyRelu # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 340
(edef LeakyRelu (Vec Float) ((X : (Vec Float)) (alpha : Float)))
;; Doing Floor # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 263
(edef Floor (Vec Float) ((X : (Vec Float))))
;; Doing SequenceLength # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/sequence/defs.cc" 323
(edef
  SequenceLength
  (Vec Integer)
  ((input_sequence : (Vec (Vec Integer)))))
(edef
  SequenceLength
  (Vec Integer)
  ((input_sequence : (Vec (Vec Float)))))
(edef
  SequenceLength
  (Vec Integer)
  ((input_sequence : (Vec (Vec (Tuple Float Float))))))
(edef
  SequenceLength
  (Vec Integer)
  ((input_sequence : (Vec (Vec Bool)))))
(edef
  SequenceLength
  (Vec Integer)
  ((input_sequence : (Vec (Vec String)))))
;; Doing GRU # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/rnn/defs.cc" 353
(edef
  GRU
  (Vec Float)
  ((X : (Vec Float))
   (W : (Vec Float))
   (R : (Vec Float))
   (B : (Vec Float))
   (sequence_lens : (Vec Integer))
   (initial_h : (Vec Float))
   (activation_alpha : (Vec Float))
   (activation_beta : (Vec Float))
   (activations : (Vec String))
   (clip : Float)
   (direction : String)
   (hidden_size : Integer)
   (linear_before_reset : Integer)))
;; Doing ReduceLogSum # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 135
(edef
  ReduceLogSum
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceLogSum
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing GlobalMaxPool # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1441
(edef GlobalMaxPool (Vec Float) ((X : (Vec Float))))
;; Doing Exp # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 498
(edef Exp (Vec Float) ((input : (Vec Float))))
;; Doing DynamicQuantizeLinear # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/quantization/defs.cc" 164
;; has body: <class 'onnx.onnx_operators_ml_pb2.FunctionProto'>
(edef DynamicQuantizeLinear (Vec Integer) ((x : (Vec Float))))
;; Doing Reshape # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 227
(edef Reshape (Vec Bool) ((data : (Vec Bool)) (shape : None)))
(edef Reshape (Vec String) ((data : (Vec String)) (shape : None)))
(edef Reshape (Vec Float) ((data : (Vec Float)) (shape : None)))
(edef Reshape (Vec Integer) ((data : (Vec Integer)) (shape : None)))
(edef
  Reshape
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float))) (shape : None)))
;; Doing GlobalAveragePool # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1437
(edef GlobalAveragePool (Vec Float) ((X : (Vec Float))))
;; Doing Or # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 74
(edef Or (Vec Bool) ((A : (Vec Bool)) (B : (Vec Bool))))
;; Doing RandomNormalLike # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 599
(edef
  RandomNormalLike
  (Vec Float)
  ((input : (Vec Bool))
   (dtype : Integer)
   (mean : Float)
   (scale : Float)
   (seed : Float)))
(edef
  RandomNormalLike
  (Vec Float)
  ((input : (Vec String))
   (dtype : Integer)
   (mean : Float)
   (scale : Float)
   (seed : Float)))
(edef
  RandomNormalLike
  (Vec Float)
  ((input : (Vec Float))
   (dtype : Integer)
   (mean : Float)
   (scale : Float)
   (seed : Float)))
(edef
  RandomNormalLike
  (Vec Float)
  ((input : (Vec Integer))
   (dtype : Integer)
   (mean : Float)
   (scale : Float)
   (seed : Float)))
(edef
  RandomNormalLike
  (Vec Float)
  ((input : (Vec (Tuple Float Float)))
   (dtype : Integer)
   (mean : Float)
   (scale : Float)
   (seed : Float)))
;; Doing Loop # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/controlflow/defs.cc" 614
(edef
  Loop
  (Vec Bool)
  ((M : (Vec Integer))
   (cond : (Vec Bool))
   (v_initial : (Vec Bool))
   (body : (Lam None(None)))))
(edef
  Loop
  (Vec String)
  ((M : (Vec Integer))
   (cond : (Vec Bool))
   (v_initial : (Vec String))
   (body : (Lam None(None)))))
(edef
  Loop
  (Vec Float)
  ((M : (Vec Integer))
   (cond : (Vec Bool))
   (v_initial : (Vec Float))
   (body : (Lam None(None)))))
(edef
  Loop
  (Vec Integer)
  ((M : (Vec Integer))
   (cond : (Vec Bool))
   (v_initial : (Vec Integer))
   (body : (Lam None(None)))))
(edef
  Loop
  (Vec (Tuple Float Float))
  ((M : (Vec Integer))
   (cond : (Vec Bool))
   (v_initial : (Vec (Tuple Float Float)))
   (body : (Lam None(None)))))
;; Doing Split # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 482
(edef
  Split
  (Vec Bool)
  ((input : (Vec Bool)) (axis : Integer) (split : (Vec Integer))))
(edef
  Split
  (Vec String)
  ((input : (Vec String)) (axis : Integer) (split : (Vec Integer))))
(edef
  Split
  (Vec Float)
  ((input : (Vec Float)) (axis : Integer) (split : (Vec Integer))))
(edef
  Split
  (Vec Integer)
  ((input : (Vec Integer)) (axis : Integer) (split : (Vec Integer))))
(edef
  Split
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float)))
   (axis : Integer)
   (split : (Vec Integer))))
;; Doing Mean # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 744
(edef Mean (Vec Float) ((data_0 : (Vec Float))))
;; Doing Mul # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 176
(edef Mul (Vec Float) ((A : (Vec Float)) (B : (Vec Float))))
(edef Mul (Vec Integer) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing Neg # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 206
(edef Neg (Vec Float) ((X : (Vec Float))))
(edef Neg (Vec Integer) ((X : (Vec Integer))))
;; Doing Not # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 158
(edef Not (Vec Bool) ((X : (Vec Bool))))
;; Doing CategoryMapper # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 173
(edef
  CategoryMapper
  (Vec String)
  ((X : (Vec String))
   (cats_int64s : (Vec Integer))
   (cats_strings : (Vec String))
   (default_int64 : Integer)
   (default_string : String)))
(edef
  CategoryMapper
  (Vec Integer)
  ((X : (Vec String))
   (cats_int64s : (Vec Integer))
   (cats_strings : (Vec String))
   (default_int64 : Integer)
   (default_string : String)))
(edef
  CategoryMapper
  (Vec String)
  ((X : (Vec Integer))
   (cats_int64s : (Vec Integer))
   (cats_strings : (Vec String))
   (default_int64 : Integer)
   (default_string : String)))
(edef
  CategoryMapper
  (Vec Integer)
  ((X : (Vec Integer))
   (cats_int64s : (Vec Integer))
   (cats_strings : (Vec String))
   (default_int64 : Integer)
   (default_string : String)))
;; Doing ReduceL1 # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 145
(edef
  ReduceL1
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceL1
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing Flatten # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1830
(edef Flatten (Vec Bool) ((input : (Vec Bool)) (axis : Integer)))
(edef Flatten (Vec String) ((input : (Vec String)) (axis : Integer)))
(edef Flatten (Vec Float) ((input : (Vec Float)) (axis : Integer)))
(edef Flatten (Vec Integer) ((input : (Vec Integer)) (axis : Integer)))
(edef
  Flatten
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float))) (axis : Integer)))
;; Doing PRelu # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 621
(edef PRelu (Vec Float) ((X : (Vec Float)) (slope : (Vec Float))))
(edef
  PRelu
  (Vec Integer)
  ((X : (Vec Integer)) (slope : (Vec Integer))))
;; Doing Unsqueeze # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1457
(edef
  Unsqueeze
  (Vec Bool)
  ((data : (Vec Bool)) (axes : (Vec Integer))))
(edef
  Unsqueeze
  (Vec String)
  ((data : (Vec String)) (axes : (Vec Integer))))
(edef
  Unsqueeze
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer))))
(edef
  Unsqueeze
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer))))
(edef
  Unsqueeze
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float))) (axes : (Vec Integer))))
;; Doing Tanh # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 542
(edef Tanh (Vec Float) ((input : (Vec Float))))
;; Doing Constant # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 171
;; Doing RNN # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/rnn/defs.cc" 232
(edef
  RNN
  (Vec Float)
  ((X : (Vec Float))
   (W : (Vec Float))
   (R : (Vec Float))
   (B : (Vec Float))
   (sequence_lens : (Vec Integer))
   (initial_h : (Vec Float))
   (activation_alpha : (Vec Float))
   (activation_beta : (Vec Float))
   (activations : (Vec String))
   (clip : Float)
   (direction : String)
   (hidden_size : Integer)))
;; Doing RandomNormal # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 469
(edef
  RandomNormal
  (Vec Float)
  ((dtype : Integer)
   (mean : Float)
   (scale : Float)
   (seed : Float)
   (shape : (Vec Integer))))
;; Doing RandomUniform # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/generator/defs.cc" 419
(edef
  RandomUniform
  (Vec Float)
  ((dtype : Integer)
   (high : Float)
   (low : Float)
   (seed : Float)
   (shape : (Vec Integer))))
;; Doing NegativeLogLikelihoodLoss # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 2167
(edef
  NegativeLogLikelihoodLoss
  (Vec Float)
  ((input : (Vec Float))
   (target : (Vec Integer))
   (weight : (Vec Float))
   (ignore_index : Integer)
   (reduction : String)))
;; Doing Abs # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 225
(edef Abs (Vec Float) ((X : (Vec Float))))
(edef Abs (Vec Integer) ((X : (Vec Integer))))
;; Doing Reciprocal # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 244
(edef Reciprocal (Vec Float) ((X : (Vec Float))))
;; Doing ReduceLogSumExp # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 140
(edef
  ReduceLogSumExp
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceLogSumExp
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing ReduceMax # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 105
(edef
  ReduceMax
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceMax
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing LessOrEqual # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/logical/defs.cc" 224
;; has body: <class 'onnx.onnx_operators_ml_pb2.FunctionProto'>
(edef LessOrEqual (Vec Bool) ((A : (Vec Float)) (B : (Vec Float))))
;; has body: <class 'onnx.onnx_operators_ml_pb2.FunctionProto'>
(edef LessOrEqual (Vec Bool) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing ScatterElements # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1066
(edef
  ScatterElements
  (Vec Bool)
  ((data : (Vec Bool))
   (indices : (Vec Integer))
   (updates : (Vec Bool))
   (axis : Integer)))
(edef
  ScatterElements
  (Vec String)
  ((data : (Vec String))
   (indices : (Vec Integer))
   (updates : (Vec String))
   (axis : Integer)))
(edef
  ScatterElements
  (Vec Float)
  ((data : (Vec Float))
   (indices : (Vec Integer))
   (updates : (Vec Float))
   (axis : Integer)))
(edef
  ScatterElements
  (Vec Integer)
  ((data : (Vec Integer))
   (indices : (Vec Integer))
   (updates : (Vec Integer))
   (axis : Integer)))
(edef
  ScatterElements
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float)))
   (indices : (Vec Integer))
   (updates : (Vec (Tuple Float Float)))
   (axis : Integer)))
;; Doing ReduceMean # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 125
(edef
  ReduceMean
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceMean
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing Cosh # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1395
(edef Cosh (Vec Float) ((input : (Vec Float))))
;; Doing ReduceMin # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 110
(edef
  ReduceMin
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceMin
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing ReduceProd # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 130
(edef
  ReduceProd
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceProd
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing Squeeze # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1353
(edef Squeeze (Vec Bool) ((data : (Vec Bool)) (axes : (Vec Integer))))
(edef
  Squeeze
  (Vec String)
  ((data : (Vec String)) (axes : (Vec Integer))))
(edef
  Squeeze
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer))))
(edef
  Squeeze
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer))))
(edef
  Squeeze
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float))) (axes : (Vec Integer))))
;; Doing CastMap # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 113
(edef
  CastMap
  (Vec String)
  ((X : (Vec (Tuple Integer String)))
   (cast_to : String)
   (map_form : String)
   (max_map : Integer)))
(edef
  CastMap
  (Vec Float)
  ((X : (Vec (Tuple Integer String)))
   (cast_to : String)
   (map_form : String)
   (max_map : Integer)))
(edef
  CastMap
  (Vec Integer)
  ((X : (Vec (Tuple Integer String)))
   (cast_to : String)
   (map_form : String)
   (max_map : Integer)))
(edef
  CastMap
  (Vec String)
  ((X : (Vec (Tuple Integer Float)))
   (cast_to : String)
   (map_form : String)
   (max_map : Integer)))
(edef
  CastMap
  (Vec Float)
  ((X : (Vec (Tuple Integer Float)))
   (cast_to : String)
   (map_form : String)
   (max_map : Integer)))
(edef
  CastMap
  (Vec Integer)
  ((X : (Vec (Tuple Integer Float)))
   (cast_to : String)
   (map_form : String)
   (max_map : Integer)))
;; Doing Selu # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 392
(edef
  Selu
  (Vec Float)
  ((X : (Vec Float)) (alpha : Float) (gamma : Float)))
;; Doing BatchNormalization # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/nn/defs.cc" 1580
(edef
  BatchNormalization
  (Vec Float)
  ((X : (Vec Float))
   (scale : (Vec Float))
   (B : (Vec Float))
   (mean : (Vec Float))
   (var : (Vec Float))
   (epsilon : Float)
   (momentum : Float)))
;; Doing Sigmoid # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 640
(edef Sigmoid (Vec Float) ((X : (Vec Float))))
;; Doing Slice # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 731
(edef
  Slice
  (Vec Bool)
  ((data : (Vec Bool))
   (starts : (Vec Integer))
   (ends : (Vec Integer))
   (axes : (Vec Integer))
   (steps : (Vec Integer))))
(edef
  Slice
  (Vec String)
  ((data : (Vec String))
   (starts : (Vec Integer))
   (ends : (Vec Integer))
   (axes : (Vec Integer))
   (steps : (Vec Integer))))
(edef
  Slice
  (Vec Float)
  ((data : (Vec Float))
   (starts : (Vec Integer))
   (ends : (Vec Integer))
   (axes : (Vec Integer))
   (steps : (Vec Integer))))
(edef
  Slice
  (Vec Integer)
  ((data : (Vec Integer))
   (starts : (Vec Integer))
   (ends : (Vec Integer))
   (axes : (Vec Integer))
   (steps : (Vec Integer))))
(edef
  Slice
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float)))
   (starts : (Vec Integer))
   (ends : (Vec Integer))
   (axes : (Vec Integer))
   (steps : (Vec Integer))))
;; Doing ReduceSumSquare # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 120
(edef
  ReduceSumSquare
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceSumSquare
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing Softmax # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 783
(edef Softmax (Vec Float) ((input : (Vec Float)) (axis : Integer)))
;; Doing Softsign # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 817
(edef Softsign (Vec Float) ((input : (Vec Float))))
;; Doing Cos # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1204
(edef Cos (Vec Float) ((input : (Vec Float))))
;; Doing SpaceToDepth # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1509
(edef
  SpaceToDepth
  (Vec Bool)
  ((input : (Vec Bool)) (blocksize : Integer)))
(edef
  SpaceToDepth
  (Vec String)
  ((input : (Vec String)) (blocksize : Integer)))
(edef
  SpaceToDepth
  (Vec Float)
  ((input : (Vec Float)) (blocksize : Integer)))
(edef
  SpaceToDepth
  (Vec Integer)
  ((input : (Vec Integer)) (blocksize : Integer)))
(edef
  SpaceToDepth
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float))) (blocksize : Integer)))
;; Doing Asinh # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1417
(edef Asinh (Vec Float) ((input : (Vec Float))))
;; Doing Tile # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1676
(edef Tile (Vec Bool) ((input : (Vec Bool)) (repeats : (Vec Integer))))
(edef
  Tile
  (Vec String)
  ((input : (Vec String)) (repeats : (Vec Integer))))
(edef
  Tile
  (Vec Float)
  ((input : (Vec Float)) (repeats : (Vec Integer))))
(edef
  Tile
  (Vec Integer)
  ((input : (Vec Integer)) (repeats : (Vec Integer))))
(edef
  Tile
  (Vec (Tuple Float Float))
  ((input : (Vec (Tuple Float Float))) (repeats : (Vec Integer))))
;; Doing ReduceL2 # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/reduction/defs.cc" 150
(edef
  ReduceL2
  (Vec Float)
  ((data : (Vec Float)) (axes : (Vec Integer)) (keepdims : Integer)))
(edef
  ReduceL2
  (Vec Integer)
  ((data : (Vec Integer)) (axes : (Vec Integer)) (keepdims : Integer)))
;; Doing Sqrt # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 301
(edef Sqrt (Vec Float) ((X : (Vec Float))))
;; Doing Log # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 520
(edef Log (Vec Float) ((input : (Vec Float))))
;; Doing Sub # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 129
(edef Sub (Vec Float) ((A : (Vec Float)) (B : (Vec Float))))
(edef Sub (Vec Integer) ((A : (Vec Integer)) (B : (Vec Integer))))
;; Doing Scaler # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/traditionalml/defs.cc" 732
(edef
  Scaler
  None
  ((X : (Vec Float)) (offset : (Vec Float)) (scale : (Vec Float))))
(edef
  Scaler
  None
  ((X : (Vec Integer)) (offset : (Vec Float)) (scale : (Vec Float))))
;; Doing Scatter # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 890
(edef
  Scatter
  (Vec Bool)
  ((data : (Vec Bool))
   (indices : (Vec Integer))
   (updates : (Vec Bool))
   (axis : Integer)))
(edef
  Scatter
  (Vec String)
  ((data : (Vec String))
   (indices : (Vec Integer))
   (updates : (Vec String))
   (axis : Integer)))
(edef
  Scatter
  (Vec Float)
  ((data : (Vec Float))
   (indices : (Vec Integer))
   (updates : (Vec Float))
   (axis : Integer)))
(edef
  Scatter
  (Vec Integer)
  ((data : (Vec Integer))
   (indices : (Vec Integer))
   (updates : (Vec Integer))
   (axis : Integer)))
(edef
  Scatter
  (Vec (Tuple Float Float))
  ((data : (Vec (Tuple Float Float)))
   (indices : (Vec Integer))
   (updates : (Vec (Tuple Float Float)))
   (axis : Integer)))
;; Doing Upsample # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/tensor/defs.cc" 1709
(edef
  Upsample
  (Vec Bool)
  ((X : (Vec Bool)) (scales : None) (mode : String)))
(edef
  Upsample
  (Vec String)
  ((X : (Vec String)) (scales : None) (mode : String)))
(edef
  Upsample
  (Vec Float)
  ((X : (Vec Float)) (scales : None) (mode : String)))
(edef
  Upsample
  (Vec Integer)
  ((X : (Vec Integer)) (scales : None) (mode : String)))
(edef
  Upsample
  (Vec (Tuple Float Float))
  ((X : (Vec (Tuple Float Float))) (scales : None) (mode : String)))
;; Doing Asin # line "/tmp/pip-req-build-ule4rbb3/onnx/defs/math/defs.cc" 1248
(edef Asin (Vec Float) ((input : (Vec Float))))
