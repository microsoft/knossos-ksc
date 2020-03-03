; ksc syntax primer

; Introduction

; The basic building block of ksc syntax is the S-expression (also are
; used in Lisp and Scheme).  That means that every language construct
; appears in nested parentheses.  In particular we can break lines
; wherever we like and and we don't need to end lines with semi-colons
; or any other form of punctuation.
;
; There are two top-level constructs: function declaration and definitions.
; Those are not required for transformations, and all constructs can appear
; in the global context, but execution needs a "main" function, which will
; then become the entry point of the compiled binary.
;
; Knossos compilers automatically include its "standard library" called
; prelude.ks (in src/runtime). The functions declared there are implemented
; in Haskell/C++ (knossos.fut/knossos.h) and will be linked with the final
; executable.

; Comments

; You might already have noticed that comments in .ks start with a
; semi-colon

#|

If you prefer block comments then use pairs of #| and |#

|#

; Types

; Knossos has four basic types:
;  * String: used mostly for debug purposes on (pr) statements
;  * Bool: used as conditions in (if) statements
;  * Integer: default to 64-bit signed integer (int64_t)
;  * Float: default to 64-bit IEEE 754 floating point numbers (double)
;
; As well as two composite types:
;  * Tuple: a collection of different types (like a C structure)
;           (Tuple Float (Tuple Integer Bool) String) -> nesting tuples
;    * Access: (get$2$3 t) -> gets second (of three) element (starts at 1)
;  * Vec: a list of single-typed elements (like Python lists)
;           (Vec (Vec Float)) -> a 2D matrix
;    * Access: (index N vec) -> return Nth element (0 < N < size(vec)-1)
;
; A Lambda type, for callable objects:
;           (Lambda (Tuple Float Float) Float) -> f({float, float}) -> float
;
; Linear Maps (LM) represent a transformation that converts one type into
; another and are used in auto-derivative functions. These are not used
; directly by user code and can be ignored (for now).

; Functions

; Functions can be forward-declared, defined (with implementation) or called
; from within another function.
;
; Declarations only expose the signature to the rest of the compilation unit
; without actually implementing the logic. This is used for the standard
; library functions (the ones implemented in Haskell/C++) as well as to help
; test arbitrary code without needing complex implementation.
;
; The syntax is: (edef name ReturnType (ArgTy1 ArgTy2 ...))
;
; Definitions implement the function directly in Knossos. If a function was
; already declared, the definition signature must match the declaration. If
; not, the declaration is implied from the definition.
;
; The syntax is: (def name ReturnType ((arg1 : ArgTy1) (arg2 : ArgTy2) ... )
;                                     (expression))
;
; Only declared or defined functions can be called. A call, like any operation,
; passes the arguments to the function definition.
;
; The syntax is: (name op1 op2 ...)
;
; Note that, even primitive operations like add or sub are function calls in
; the Knossos IR. The best places to find a full list of supported functions
; are prelude.ks and the Prim module.
;
; The standard library names are split into three parts: prefix$name@types
;
; The prefix is reserved for auto-generated functions (like derivatives) and
; you shouldn't need to call those functions directly.
;
; The name represents the operation itself, like add, mul, dot, gt, lt, etc.
;
; The suffix represents the types involved, for example 'f' for float and 'i'
; for integers. So, 'add@ii' "adds" two integers, returning the sum.

; The following declares a function of two variables x and y (both
; Integers) which returns an Integer.
(edef f1 Integer (Integer Integer))

; Then it defines that function, with its implementation
(def f1 Integer ((x : Integer) (y : Integer))
     (add@ii x y))

; Python equivalent
;
; def f1(x : int, y : int) -> int:
;     return x + y

; A call to that function would simply be:
(fun 10 20)

; The following defines a function that takes five arguments (a to e)
; each of type Integer and returns an Integer.  It performs the
; arithmetic operation ((a*b) + (-c)) - (d/e).
(def f2 Integer
     ((a : Integer)
      (b : Integer)
      (c : Integer)
      (d : Integer)
      (e : Integer))
     (sub@ii (add@ii (mul@ii a b) (neg@i c)) (div@ii d e)))

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
; The return type is the type returned by the basic blocks, which must be
; identical for consistency.
;
(def if_example Integer ((b1 : Bool) (b2 : Bool) (a : Integer))
     (if (or b1 b2)
         (add@ii a 10)
         (sub@ii a 10)))

; Python equivalent
;
; The equivalent in Python would most commonly be written like
; if_example1 below:
; def if_example1(b1 : bool, b2 : bool, a : Integer) -> int:
;     if b1 or b2:
;         return a + 10
;     else:
;         return a - 10
;
; On the other hand, Knossos has if expressions
; rather than if statements so a more direct translation would use
; Python's ternary "... if ... else ..." expression form, as shown in
; if_example2:
; def if_example2(b1 : bool, b2 : bool, a : Integer) -> int:
;     return a + 10 if b1 or b2 else a - 10


; Knossos constants, variables and let bindings
;
; Constants are literals in code:
;  * Integer literals are numeric literals (without the decimal point)
;  * Float literals are numeric literals (and must contain a decimal point)
;  * Bool literals are "true" and "false"
;  * String literals are "quoted strings"
;
; Variables are:
;  * Declared in function definitions, ex: "(x : Float)" to use in the function
;  * Defined in Let bindings, see below, with an initialiser
;  * Used inside functions, by using its name directly
;
; To create new variables use let bindings. The scope of the variable remains
; active after the let binding and is destroyed when the scope encompassing let
; is left. There can be any number of let bindings in that way.
;
; Knossos allows name shadowing, so you can reuse the name of an earlier
; variable in a subsequent let binding.

; The following code defines four variables, b2, i2, f2 and s2, in one let
; statement, and those variables are used on the following if statement:
(def let_and_types Float ((b : Bool) (s : String) (i : Integer) (f : Float) (v : Vec Float))
     (let ((b2 (or b false))
           (i2 (add@ii i 10))
           (f2 (add f 10.0))
           (s2 "Hello"))
       (if (and (gte@ii i 0) (lt@ii i (size v)))
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
; The syntax is: (build N (Lambda))
;
; The lambda can only accept one argument, an Integer, the induction variable,
; which "build" iterates from 0 to N, calling the lambda with that value.

; This example creates a vector of length n whose ith index is i squared as float:
(def build_example (Vec Float) (n : Integer)
     (build n (lam (ni : Integer) (to_float (mul@ii ni ni)))))

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
        (add@ii n (triangle0 (sub@ii n 1)))))

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
         (triangle (add@ii acc n) (sub@ii n 1))))

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
(def fwd$my_log Float ((x : Float) (dx : Float)) (div@ff dx x))
(def rev$my_log Float ((x : Float) (d_dmy_log : Float)) (div@ff d_dmy_log x))
(edef Dt$my_log (Tuple Float (LM Float Float)) (Float))
