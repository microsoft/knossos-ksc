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

; Python equivalent
;
; def f1(x : int, y : int) -> int:
;     return x + y


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
     (sub (add (mul a b) (neg c)) (div d e)))

; Python equivalent
;
; def f2(a : int, b : int, c : int, d : int, e : int) -> int:
;     return ((a * b) + -c) - (d / e)


; Conditionals

; A conditional ("if") looks like a function call with three
; arguments.  The first is the condition, the second is the expression
; to be evaluated if the condition is true, and the third is the
; expression to be evaluated if the condition is false.
;
; The Knossos boolean type is called "Bool"
(def if_example Integer ((b1 : Bool) (b2 : Bool) (a : Integer))
     (if (or b1 b2)
         (add a 10)
         (sub a 10)))

; Python equivalent
;
; The equivalent in Python would most commonly be written like
; if_example1 below.  On the other hand, Knossos has if expressions
; rather than if statements so a more direct translation would use
; Python's ternary "... if ... else ..." expression form, as shown in
; if_example2.
;
; def if_example1(b1 : bool, b2 : bool, a : Integer) -> int:
;     if b1 or b2:
;         return a + 10
;     else:
;         return a - 10
;
; def if_example2(b1 : bool, b2 : bool, a : Integer) -> int:
;     return a + 10 if b1 or b2 else a - 10


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
       (if (and (gte i 0) (lt i (size v)))
           (index i v)
           f2)))

; Python equivalent
;
; def let_and_types(b : bool, s : str, i : int, f : double, v : List[double]) -> double:
;     b2 = b or False
;     i2 = i + 10
;     f2 = f + 10.0
;     s2 = "Hello"
;
;     if i >= 0 and i < len(v):
;         return v[i]
;     else:
;         return f2


; Vectors are created with the "build" function.
;
; This example creates a vector of length n whose ith index is i
; squared.
;
; If you are already familiar with lambdas you can read this as "build
; a vector of length n where the element at position i is given by the
; lambda expression applied to i".
(def build_example (Vec Float) (n : Integer)
     (build n (lam (ni : Integer) (to_float (mul ni ni)))))

; Python equivalent
;
; def build_example(n : int) -> List[double]:
;     return list(float(ni * ni) for ni in range(n))


; Looping constructs

; Knossos does not have for loops or while loops.  Instead we use
; recursion.  A recursive function to calculate the nth triangle
; number might be implemented as follows.
(def triangle0 Integer (n : Integer)
    (if (eq n 0)
        0
        (add n (triangle0 (sub n 1)))))

; Python equivalent
;
; The direct Python equivalent would be triangle0 below.
;
; def triangle0(n : int) -> int:
;     if n == 0:
;         return 0
;     else:
;         return n + triangle0(n - 1)


; There is a problem with both the Knossos and the Python
; implementations of triangle0.  They are recursive but not tail
; recursive.  Therefore they consume stack space.  One tends to right
; such functions in tail-recursive form if possible.  For more
; information on tail recursion see the Wikipedia article.
;
; https://en.wikipedia.org/wiki/Tail_call
;
; A triangle number calculation function in tail recursive form is
; given in triangle below.  The "acc" argument is the loop
; accumulator.  To calculate the nth triangle number one starts the
; accumulator at zero by calling
;
;     (triangle 0 n)
(def triangle Integer ((acc : Integer) (n : Integer))
     (if (eq n 0)
         0
         (triangle (add acc n) (sub n 1))))

; Python equivalent
;
; The direct Python equivalent is triangle.  One would generally not
; write it in Python because Python lacks tail call elimination and
; the function would inefficiently consume stack space.  Normally a
; Python programmer would write an imperative-style program like
; triangle_imperative or a functional-style program like
; triangle_functional.
;
; def triangle(acc : int, n : int) -> int:
;     if n == 0:
;         return 0
;     else:
;         else triangle(acc + n, n - 1)
;
; def triangle_imperative(n : int) -> int:
;     acc = 0
;     for i in range(n, 0, -1):
;         acc = acc + i
;     return acc
;
; def triangle_functional(n : int) -> int:
;     return sum(range(n, 0, -1))


; fold is a primitive that implements a particular recursion pattern
; so that you don't have to write it out by hand.  It is written as
;
;     (fold f s0 v)
;
; where s0 is the initial state, f maps from state and element to
; state and v is a vector to loop over.
;
; This example calculates the sum of the elements in a vector.
(def fold_example Float (v : Vec Float)
     (fold (lam (s_vi : Tuple Float Float)
                (let ((s (get$1$2 s_vi))
                      (vi (get$2$2 s_vi)))
                  (add s vi)))
           0.0
           v))

; Python equivalent
;
;     def fold(f, s0, v):
;         s = s0
;         for vi in v:
;             s = f(s, vi)
;         return s


; If there's a main function then it will become the main function of
; the resulting C++ file and thus the entry point of the compiled
; binary.
;
; You can use the pr function for printing values.
(def main Integer ()
     (pr "Hello world"))

; Python equivalent
;
; def main() -> integer:
;     print("Hello world")
;
; if __name__ == '__main__': main()


; If you want to call a function defined in an external C module you
; can provide its name and type with an "edef" declaration, and then
; use it as though it were a function you had defined yourself.  An
; "edef" is somewhat like a C function declaration.  You can even define
; manual derivatives for your function.
(edef my_log Float (Float))
(edef D$my_log (LM Float Float) (Float))
(def fwd$my_log Float ((x : Float) (dx : Float)) (div dx x))
(def rev$my_log Float ((x : Float) (d_dmy_log : Float)) (div d_dmy_log x))
(edef Dt$my_log (Tuple Float (LM Float Float)) (Float))
