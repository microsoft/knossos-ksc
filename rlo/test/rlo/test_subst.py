# fmt: off
from rlo.expression import Expression, EF
from ksc.type import Type
from rlo.utils import Assert

def test_replace_subtree_renaming():
    e = EF.Let("y", "other", EF.Add("x", "y"))
    assert e.nodes[4] == Expression.Variable("x")
    s = e.replace_subtree(4, EF.Add("y", 1))
    assert s == EF.Let("z", "other", EF.Add(EF.Add("y", 1), "z"))
    # In this next, y (in the value substituted) is erroneously captured
    assert s != EF.Let("y", "other", EF.Add(EF.Add("y", 1), "y"))

    e2 = EF.Let("y", "other", EF.Add(EF.Let("var0", 3, EF.Add("var0", "y")), EF.Add("x", "y")))
    assert e2.free_var_names == frozenset(["other", "x"])
    assert e2.nodes[11] == Expression.Variable("x")
    s2 = e2.replace_subtree(11, EF.Add("y", 1))
    # The existing y should be renamed - either to var1 (better), or at least y to var0 and then var0 to var1.
    assert s2 == EF.Let("var1", "other", EF.Add(EF.Let("var0", 3, EF.Add("var0", "var1")), EF.Add(EF.Add("y", 1), "var1")))
    # Either of these two is OK, tho the second probably involved less copying
    assert str(s2) in ["(let (var0 other) (add (let (var1 3) (add var1 var0)) (add (add y 1) var0)))",
                       "(let (var1 other) (add (let (var0 3) (add var0 var1)) (add (add y 1) var1)))"]
    # The wrong case, where we rename y to conflict with var0
    assert s2 != EF.Let("var0", "other", EF.Add(EF.Let("var0", 3, EF.Add("var0", "var0")), EF.Add(EF.Add("y", 1), "var0")))

def test_cav_helper_does_nothing():
    e = EF.Let("y", "other", EF.Let("var0", 5, EF.Add("var0", 1)) + EF.Add("y", 1))
    # Rename free/external y. y is bound within the expression, so does nothing.
    s = e._cav_helper(None, subst={"y": "var0"})
    assert s == e # Trivially alpha-equivalent
    assert str(s) == str(e) # No renaming actually performed
    assert s is e

def test_cav_helper_preserves_type():
    e = EF.Let("f", EF.Lam(Expression.Variable("x", Type.Integer), EF.Add("x", "y")), "f")
    s = e._cav_helper(None, {"y": EF.Mul("x", 2)}) # A different, outer, x
    assert s == EF.Let("f", EF.Lam(Expression.Variable("x2", Type.Integer), EF.Add("x2", EF.Mul("x", 2))), "f")
    assert s != EF.Let("f", EF.Lam(Expression.Variable("x", Type.Integer), EF.Add("x", EF.Mul("x", 2))), "f")

def test_replace_subtree_renames_in_target():
    e = EF.Let("x", 5, EF.Apply("f", "x"))
    # Suppose we want to inline "f" as being another (function) variable "x"
    # (This is a simpler case than inlining an "f" with a free "x", but demonstrates the same problem)
    s = e.replace_subtree(3, lambda e: Assert(e.op == "apply" and e.left.name=="f").then_return(EF.Apply("x", e.right)), Expression.Variable("x"))
    assert s == EF.Let("x2", 5, EF.Apply("x", "x2"))
    # If we didn't rename inside the "existing" Expression passed into the lambda
    assert s != EF.Let("x", 5, EF.Apply("x", "x"))

def test_replace_subtree_rebinding():
    e = EF.Let("x", EF.Add("x", 1), EF.Mul("x", 2))
    assert e.nodes[5].op == "mul" and e.nodes[6].op == "variable"
    s = e.replace_subtree(6, EF.Add("x", 1))
    assert s == EF.Let("x2", EF.Add("x", 1), EF.Mul(EF.Add("x", 1), 2)) # overall result: (x+1)*2
    # If we forgot to rename the bound x, and allowed capture:
    assert s != EF.Let("x", EF.Add("x", 1), EF.Mul(EF.Add("x", 1), 2)) # overall result would be: ((x+1)+1)*2

    e2 = EF.Let("x", "x", EF.Add("x", 1))
    # Here, inlining the use of the bound "x" with the outer "x" requires renaming the outer "x"
    # (Is this too much of an implementation detail? It'd be fine to avoid the tree traversal and construction)
    assert e2.nodes[3].op == "add" and e2.nodes[4].op == "variable"
    s2 = e2.replace_subtree(4, Expression.Variable("x"))
    assert s2 == EF.Let("x2", "x", EF.Add("x", 1))
    assert s2 != EF.Let("x", "x", EF.Add("x", 1))
