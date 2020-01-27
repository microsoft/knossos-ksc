; ksc syntax primer

; Basic syntax, definining and calling functions

; The basic building block of ksc syntax is the S-expression (also are
; used in Lisp and Scheme).  That means that every language construct
; appears in nested parentheses.  In particular we can break lines
; wherever we like and and we don't need to end lines with semi-colons
; or any other form of punctuation.
;
; The following defines a function of two variables x and y (both
; Integers) which returns an Integer.

(def f1 Integer ((x : Integer) (y : Integer))
     (add x y))

; Comments

; You might already have noticed that comments in .ks start with a
; semi-colon

#|

If you prefer block comments then use pairs of #| and |#

|#

; A bigger example

; The syntax of nested function calls looks different in .ksc to most
; other languages. It does not use infix operators for arithmetic.
; Instead there are functions called "add", "sub", "mul", "div", etc.,
; suffixed with the type of argument that they take (i for Integer, f
; for Float). The best places to find a full list of supported
; functions are prelude.ks and the Prim module.
;
; The following defines a function that takes five arguments (a to e)
; each of type Integer and returns an Integer.  It performs the
; arithmetic operation ((a*b) + (-c)) - (d/e).
(def f2 Integer
     ((a : Integer)
      (b : Integer)
      (c : Integer)
      (d : Integer)
      (e : Integer))
     (sub@ii (add (mul@ii a b) (neg c)) (div@ii d e)))

; Conditionals

; A conditional ("if statement") looks like a function call with three
; arguments.  The first is the condition, the second is the expression
; to be evaluated if the condition is true, and the third is the
; expression to be evaluated if the condition is false.
;
; The Knossos boolean type is called "Bool"
(def if_example Integer ((b1 : Bool) (b2 : Bool) (a : Integer))
     (if (or b1 b2)
         (add a 10)
         (sub@ii a 10)))

; Knossos types, constants, let bindings

; Knossos has the following types: String, Bool, Integer, Float, Tuple
; and Vec.
;
; "Float" compiles to C++ double and "Vec" compiles to ks::vec from
; the Knossos C++ runtime.
;
; * Integer literals are numeric literals
;
; * Float literals are numeric literals (and must contain a decimal point)
;
; * Bool literals are "true" and "false"
;
; * Vec is indexed with "index" (and created with "build" (see
;   below)).  Please note that the order of arguments is "index i v"
;   where v is the vector being indexed and i is the index.  This is
;   the opposite way round from what you might expect if you are used
;   to "v[i]" notation.
;
; To create new variables use "let"
(def let_and_types Float ((b : Bool) (s : String) (i : Integer) (f : Float) (v : Vec Float))
     (let ((b2 (or b false))
           (i2 (add i 10))
           (f2 (add f 10.0))
           (s2 "Hello"))
       (if (and (gte@ii i 0) (lt@ii i (size v)))
           (index i v)
           f2)))

; Vectors are created with the "build" function.
;
; This example creates a vector of length n whose ith index is i
; squared.
;
; If you are already familiar with lambdas you can read this as "build
; a vector of length n where the element at position i is given by the
; lambda expression applied to i".
(def build_example (Vec Float) (n : Integer)
     (build n (lam (ni : Integer) (to_float (mul@ii ni ni)))))

; Knossos does not have for loops or while loops.  Instead we use
; recursion.
(def triangle Integer (n : Integer)
     (if (eq n 0)
         0
         (add n (triangle (sub@ii n 1)))))

; fold is a primitive that implements a particular recursion pattern
; so that you don't have to write it out by hand.  It is written as
;
;     (fold f s0 v)
;
; where s0 is the initial state, f maps from state and element to
; state and v is a vector to loop over.  It performs the equivalent of
; the following Python code
;
;     def fold(f, s0, v):
;         s = s0
;         for vi in v:
;             s = f(s, vi)
;         return s
;
; This example calculates the sum of the elements in a vector.
(def fold_example Float (v : Vec Float)
     (fold (lam (s_vi : Tuple Float Float)
                (let ((s (get$1$2 s_vi))
                      (vi (get$2$2 s_vi)))
                  (add s vi)))
           0.0
           v))

; If there's a main function then it will become the main function of
; the resulting C++ file and thus the entry point of the compiled
; binary.
;
; You can use the pr function for printing values.
(def main Integer ()
     (pr "Hello world"))
