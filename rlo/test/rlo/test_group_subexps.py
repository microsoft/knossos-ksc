# fmt: off
from rlo.expression import Expression, EF
from rlo.group_subexps import nodes_and_groups
from ksc.type import Type

def test1():
    e = EF.Let("f", EF.Lam(Expression.Variable("x", Type.Integer), EF.Add("x", "x")),
            EF.Let("g", EF.Lam(Expression.Variable("y", Type.Integer), EF.Add("y", "y")),
                EF.Apply("f", 3) + EF.Apply("g", 5)))
    nodes, groups = nodes_and_groups(e)
    groups = set(frozenset(l) for l in groups)

    assert nodes[0].op == "let" and nodes[1].name == "f" and nodes[2].op == "lam"
    assert nodes[7].op == "let" and nodes[8].name == "g" and nodes[9].op == "lam"

    # Remove raises an exception if the specified sets are not present.
    # So, first remove the expected groups, then check there are no significant groups remaining.

    # The two lambdas, node 2 and 9, should be considered equal
    groups.remove({2,9})

    assert nodes[3].name == "x" and nodes[5].name == "x" and nodes[6].name == "x"
    # We include the binding occurrence as a node marked equal to its uses.
    # So all three x's should be equal.
    groups.remove({3, 5, 6})

    assert nodes[10].name == "y" and nodes[12].name == "y" and nodes[13].name == "y"
    groups.remove({10, 12, 13})

    assert nodes[15].op == "apply" and nodes[18].op == "apply"
    groups.remove({1, 16}) # The two f's
    groups.remove({8, 19}) # The two g's
    assert all(len(g)==1 for g in groups) # Nothing significant left.

def test2():
    x = Expression.Variable("x")
    e = EF.Let(x, 3, EF.Add(x+x, EF.Add(EF.Let(x, 3, (x+x)), (x+x))))
    nodes, groups = nodes_and_groups(e)
    groups = set(frozenset(l) for l in groups)

    assert nodes[0].op == "let"
    outer_x = nodes[0].first
    assert nodes[8].op == "let"
    inner_x = nodes[8].first

    assert nodes[4] == outer_x + outer_x
    assert nodes[11] == inner_x + inner_x
    assert nodes[14] == outer_x + outer_x

    # Remove raises if its argument isn't present.
    groups.remove({4,14}) # not equivalent to 11
    groups.remove({1, 5,6, 15,16}) # binding occurrence of outer x, plus operands to the outer two x+x's
    groups.remove({9, 12, 13}) # binding occurrence of inner x, plus operands to the inner x+x

    assert nodes[2] == Expression.Constant(3) and nodes[10] == Expression.Constant(3)
    groups.remove({2, 10})
    assert all(len(g)==1 for g in groups) # Nothing significant left.

def test3():
    x = Expression.Variable("x", Type.Integer)
    y = Expression.Variable("y", Type.Integer)
    e = EF.Let("f", EF.Lam(x, EF.Add("x", EF.Add("y", 3))),
            EF.Let("g", EF.Lam(y, EF.Add("y", 3)),
                EF.Add("y", 3)))
    nodes, groups = nodes_and_groups(e)
    groups = set(frozenset(l) for l in groups)

    assert nodes[0].op == "let" and nodes[0].first.name == "f"
    assert nodes[9].op == "let" and nodes[9].first.name == "g"
    # The first and last y's are free in the whole expression so cannot be renamed:
    assert nodes[6] == EF.Add("y", 3) and nodes[16] == EF.Add("y", 3)
    assert nodes[11].op == "lam" and nodes[13] == EF.Add(nodes[12], 3)

    # Remove raises if its argument isn't present.
    groups.remove({6, 16}) # The two y+3 for the free 'y'
    groups.remove({7, 17}) # The two free y's (first children of previous)
    groups.remove({12, 14}) # The inner y (bound)
    groups.remove({3, 5}) # The two x's in the first lam

    three = Expression.Constant(3)
    assert nodes[8] == three and nodes[15] == three and nodes[18] == three
    groups.remove({8, 15, 18})
    assert all(len(g) == 1 for g in groups) # Nothing significant left.

def test4():
    x = Expression.Variable("x", Type.Integer)
    e = EF.Lam(x, EF.Add(x+x, EF.Add(EF.Let("y", 3, EF.Add(x+x,"y")), x+x)))
    nodes, groups = nodes_and_groups(e)
    groups = set(frozenset(l) for l in groups)

    assert nodes[3] == x+x and nodes[11] == x+x and nodes[15] == x+x
    # In nodes[9], we are inside the let y=... so x has a different deBruijn number to 3/13
    groups.remove({3, 11, 15}) # Remove raises if its argument isn't present.

    groups.remove({1, 4,5, 12,13, 16,17}) # All the x's in the previous (+binder)

    assert nodes[8].op == "variable" and nodes[14] == nodes[8] # The bound y
    groups.remove({8, 14})

    assert all(len(g) == 1 for g in groups) # Nothing significant left.

def test_rebinding():
    e = EF.Let("x", "x", 3)
    _nodes, groups = nodes_and_groups(e)

    # The two x's are different, so there should be no groups.
    assert all(len(g)==1 for g in groups)
