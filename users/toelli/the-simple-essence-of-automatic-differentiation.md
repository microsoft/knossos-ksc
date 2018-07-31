# The Simple Essence of Automatic Differentiation

Conal observes that, for vector spaces `a` and `b`, the derivitive of
a function `f :: a -> b` is correctly calculated as a value in the
space

    newtype D k a b = D (a -> (b, a `k` b))

The chain rule is then the composition rule in `D`, i.e.

    D g . D f = D (\a -> let {(b, f') = f a; (c, g') = g b} in (c, g' . f'))

For suitable choices of `k`, `D k` is an instance of various
typeclasses like `Category`, `Monoidal`, `Cartesian`, `Cocartesian`
and `NumCat`.  If one programs polymorphically over those typeclasses
then one can write expressions that represent arithmetic and vector
calculations.  The expressions calculate derivatives when instantiated
at different values of `k`.

This is extremely convenient in the sense that all the infrastructure
machinery is thus already provided in Haskell.  However, it is
extremely inconvenient to write programs in this form.  The function

    magSqr x y = let sqr a = a * a in sqr x + sqr y

is written as

    magSqr = addC . (mulC . (exl ^ exl) ^ mulC . (exr ^ exr))

(for a suitable argument-duplicating operation `^`).  I think the type
of `magSqr` would be

    (NumCat k a, Cartesian k) => (a, a) `k` a

which can be instantiated as

    (Float, Float) -> Float

where it will calculate the value of the function, or as

    D (-o) (Float, Float) Float

where it will calculate the value of the function *and* its
derivative.  (The lollipop symbol `-o` is supposed to represent some
form of linear function space but its definition seems to be left
vague.  NB this is *not* a type theoretic linear function space, but
an algebraic one.)

There is some hope that a source preprocessor will allow us to write
readable code.  On page 10 there is a reference to "translating the
λ-calculus to categorical form" but I'm still worried that the exact
properties of this translation may be critical to getting good
performance.  As we know from the case of Haskell's Arrows, different
desugarings can have significantly different performance.

Conal observes that the associative order of the operations will
determine the mode of the automatic differentiation that is applied to
calculate the derivative (Section 12, p17).  If the compositions are
fully right-associated the calculation will be performed in what
amounts to forward mode and if fully left-associated then the
computation will be performed in backward mode.  Mixed associations
give rise to mixed modes of computation.  He explains ingenious CPS
trick that can force the entire computation to run in either forward
or backward mode.  If instead of interpreting the derivative in

    a `k` b

you instead use

    (b `k` r) -> (a `k` r)

results in converting *all* composition into a fully left-associated
form, that is, calculating reverse mode AD.  Similarly if you use

    (r `k` a) -> (r `k` b)

("beginning passing style"?) then all compositions are converted into
fully right-associated form, calculating forward mode AD.

(This is essentially the same trick as [difference
lists](http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/) (using `[a]
-> [a]`) and [Asymptotic Improvement of Computations over Free
Monads](http://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf)
(using `Codensity m a = (a -> m b) -> m b`).)

Conal has another modular trick up his sleeve for calculating
gradients.  Note that what we comes out as the derivative of a
function `f :: a -> Float` is a linear map `a -> Float`.  This is not
terribly useful.  We wanted a vector of `Float`s.  He writes a type
for the dual space

    newtype Dual k a b = Dual (b `k` a)

and shows we can interpret the calculation in this type.  When `k =
->` and and `b = Float` we end up with a

    Float -> a

We apply that to `1` and get out an `a` which is the gradient vector.
