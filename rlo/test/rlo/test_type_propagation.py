import pytest

from rlo import expr_sets
from testutils import parse_expr_typed


@pytest.mark.parametrize(
    "expr_set_name",
    [
        "fewer_simplify_expressions",
        "binding_simplify_expressions",
        "bindgen_12",
        "bindgen_test",
        "summations(n=100,min_terms=3,max_terms=10)",
        "ksc/blas/blas_train.kso",
        "ksc/blas/blas_test.kso",
        "ksc/gmm/gmm_train.kso",
        "ksc/gmm/gmm_test.kso",
    ],
)
def test_all_nodes_have_types(expr_set_name):
    expr_set = expr_sets.get_expression_set(expr_set_name)
    for _, exprenv in expr_set.named_exprenvs():
        for node in exprenv.expr.nodes:
            assert node.type is not None


def test_parse_expr():
    expr_str = """
(let 
  (scal (lam (var0 : (Tuple Float (Vec Float))) 
    (let 
      (x (get$2$2 var0)) 
      (let 
        (a (get$1$2 var0)) 
        (let 
          (n (size x)) 
          (build n (lam (i : Integer) 
            (mul a 
              (index i x))))))))) 
  (let 
    (avxvy (lam (var0 : (Tuple Float (Vec Float) (Vec Float))) 
      (let 
        (y (get$3$3 var0)) 
        (let 
          (x (get$2$3 var0)) 
          (let 
            (a (get$1$3 var0)) 
            (build (size x) (lam (i : Integer) 
              (build (size y) (lam (j : Integer) 
                (mul 
                  (index i (scal a x)) 
                  (index j y))))))))))) avxvy))"""
    exprenv = parse_expr_typed(expr_str)
    for node in exprenv.expr.nodes:
        assert node.type is not None
