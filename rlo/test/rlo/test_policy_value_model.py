import numpy as np
import pytest
import tensorflow as tf


from rlo.policy_value_model import PolicyValueModel, negative_entropy_regularizer
from rlo import rewrites
from ksc.type import Type
from rlo import utils
from mock_dataset import mock_policy_value_dataset
from testutils import parse_expr_typed


def learned_einsum(output_dims):
    return tf.keras.Sequential(
        [
            tf.keras.layers.Dense(np.prod(output_dims)),
            tf.keras.layers.Lambda(lambda x: tf.reshape(x, (-1, *output_dims))),
        ]
    )


class SimplePolicyValueModel(PolicyValueModel):
    def __init__(self, num_time_heads, num_rules, **kwargs):
        super().__init__(num_time_heads=num_time_heads, **kwargs)
        self.einsum = learned_einsum((num_rules, num_time_heads))

    def _compute_scores(self, features):
        return self.einsum(features)


def get_policy_value_model_wrapper(sparse, num_time_heads, rules, eager):
    return SimplePolicyValueModel.create_wrapper(
        seed=0,
        rules=rules,
        sparse=sparse,
        model_hypers=dict(
            hidden_dim=5,
            output_hidden_dim=6,
            num_propagations=2,
            num_rules=len(rules),
            message_from_sender_receiver=False,
        ),
        num_time_heads=num_time_heads,
        use_subtree_match_edges=True,
        cost_norm="log",
        eager=eager,
    )


@pytest.mark.parametrize("sparse", [False, True])
@pytest.mark.parametrize("rules_name", ["simplify_rules", "binding_rules"])
@pytest.mark.parametrize("num_time_heads", [7, 11])
@pytest.mark.parametrize("eager", [False, True])
def test_policy_value_evaluation_shapes(sparse, num_time_heads, rules_name, eager):
    rules = rewrites.get_rules(rules_name)

    test_exprs = [
        parse_expr_typed(e_str, default_var_type=Type.Integer)
        for e_str in [
            "(let ((a (add x 3))) (add (mul a a) y))",
            "(build m (lam (i : Integer) (build n (lam (j : Integer) (add (to_float i) (to_float j))))))",
            "(let ((t (tuple x y z))) (add t t))",
        ]
    ]

    def check(evs, expected_num_nodes):
        values, log_probs = evs
        assert values.shape == (num_time_heads,)
        assert log_probs.shape == (expected_num_nodes, len(rules), num_time_heads)

    model = get_policy_value_model_wrapper(sparse, num_time_heads, rules, eager)

    # first evaluate as a batch
    evaluation = model.evaluate_all_time_left(test_exprs)
    assert len(evaluation) == len(test_exprs)
    for exprenv, evs in zip(test_exprs, evaluation):
        check(evs, exprenv.expr.num_nodes)

    # next evaluate each expression on its own
    for exprenv in test_exprs:
        evaluation = model.evaluate_all_time_left([exprenv])
        evs = utils.single_elem(evaluation)
        check(evs, exprenv.expr.num_nodes)


@pytest.mark.parametrize("sparse", [False, True])
@pytest.mark.parametrize("rules_name", ["simplify_rules", "binding_rules"])
@pytest.mark.parametrize("num_time_heads", [7, 11])
@pytest.mark.parametrize("eager", [False, True])
def test_mask_log_probs(sparse, num_time_heads, rules_name, eager):
    rules = rewrites.get_rules(rules_name)
    model = get_policy_value_model_wrapper(sparse, num_time_heads, rules, eager)

    num_rules = len(rules)
    exprs = [
        parse_expr_typed("(div (mul x x) x)"),
        parse_expr_typed("(div (div 1.0 x) (add (div 1.0 x) 1.0))"),
        parse_expr_typed("(mul x (add (div 1.0 x) 1.0))"),
    ]
    evaluation = model.evaluate_all_time_left(exprs)
    for exprenv, evs in zip(exprs, evaluation):
        num_nodes = exprenv.expr.num_nodes
        actions = set(
            (a.node_id, rules.id_for_rule(a.rule))
            for a in rules.get_all_rewrites(exprenv)
        )
        print(actions)
        _, log_probs = evs
        # iterate over all time_left
        for t in range(num_time_heads):
            probs = np.exp(log_probs[:, :, t])
            np.testing.assert_allclose(np.sum(probs), 1.0, rtol=1e-4)
            for node_id in range(num_nodes):
                for rule_id in range(num_rules):
                    if (node_id, rule_id) in actions:
                        assert probs[node_id, rule_id] > 0.0
                    else:
                        np.testing.assert_equal(probs[node_id, rule_id], 0.0)


@pytest.mark.parametrize("sparse", [False, True])
@pytest.mark.parametrize("num_time_heads", [7, 11])
@pytest.mark.parametrize("eager", [False, True])
def test_distillation(sparse, num_time_heads, eager):
    ds, rules = mock_policy_value_dataset()
    model = get_policy_value_model_wrapper(sparse, num_time_heads, rules, eager)

    kwargs = {"max_nodes": 100} if sparse else {"batch_size": 2}
    batches = list(model.create_dataset(ds.get_examples(), **kwargs))
    for batch in batches:
        model.train_on_batch(batch)
        print(model.evaluate_loss(batch))
    print(f"Trained on {len(batches)} batches")


def test_negative_entropy_regularizer():
    probs = np.transpose(
        [
            [
                [0.0, 0.2, 0.0],
                [0.4, 0.0, 0.4],
                [0.0, 0.0, 0.3],
                [0.0, 0.1, 0.1],
                [0.1, 0.1, 0.0],
                [0.1, 0.2, 0.0],
            ],
            [
                [0.0, 0.2, 0.2],
                [0.6, 0.0, 0.0],
                [0.0, 0.1, 0.2],
                [0.0, 0.1, 0.1],
                [0.3, 0.0, 0.0],
                [0.1, 0.1, 0.0],
            ],
        ],
        (1, 2, 0),
    )
    segment_ids = [0, 0, 1, 1, 1, 1]
    assert len(probs) == len(segment_ids)
    np.testing.assert_equal(
        tf.reduce_sum(
            tf.math.segment_sum(tf.constant(probs), tf.constant(segment_ids)), axis=1
        ).numpy(),
        np.ones((2, 2)),
    )

    def negative_entropy(probs):
        return (np.where(probs > 0, probs * np.log(probs), 0.0)).sum()

    expected = np.mean(
        [
            negative_entropy(probs[s, :, t])
            for s in [slice(0, 2), slice(2, 6)]
            for t in range(2)
        ]
    )
    np.testing.assert_equal(
        negative_entropy_regularizer(
            tf.constant(np.log(probs)), tf.constant([0, 2, 6])
        ).numpy(),
        expected,
    )
