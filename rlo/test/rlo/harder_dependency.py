from rlo.expression import Expression, EF
from ksc.type import Type

a = Expression.Variable("a")
b = Expression.Variable("b")
c = Expression.Variable("c")
d = Expression.Variable("d")
e = Expression.Variable("e")
f = Expression.Variable("f")
x = Expression.Variable("x", Type.Float)

f_a = EF.Lam(x, 3.0 * x)  # type: ignore
f_b = EF.Lam(x, 1.0 / (x * x))  # type: ignore
f_c = EF.Lam(x, (2.0 * EF.Apply(b, x)) - (EF.Apply(a, x) + 3.0))  # type: ignore
f_d = EF.Lam(x, 4.0 + EF.Apply(b, (2.0 * EF.Apply(a, x))))  # type: ignore
f_e = EF.Lam(x, EF.Apply(d, EF.Apply(c, EF.Apply(b, x))))  # type: ignore
f_f = EF.Lam(x, x / (x * x))  # type: ignore
