# fmt: off
import os
import numpy as np
import pytest
import tempfile

from rlo import cost_normalizers
from rlo import regressor
from ksc.type import Type
from testutils import parse_expr_typed

def get_test_regressor(seed=1,
                       use_subtree_match_edges=True,
                       hidden_dim=100,
                       output_hidden_dim=80,
                       num_propagations=5,
                       num_time_heads=10,
                       num_gnn_blocks=1,
                       sparse=False,
                       aggregation_over_edge_types="sum",
                       message_from_sender_receiver=False,
                       **kwargs):
    model_hypers = dict(
        hidden_dim=hidden_dim,
        output_hidden_dim=output_hidden_dim,
        num_propagations=num_propagations,
        recurrent_dropout=kwargs.pop("recurrent_dropout", 0.5),
        output_dropout=kwargs.pop("output_dropout", 0.5),
        num_gnn_blocks=num_gnn_blocks,
        stacked_gnn_double_hidden=False,
        aggregation_over_edge_types=aggregation_over_edge_types,
        message_from_sender_receiver=message_from_sender_receiver,
    )

    kwargs.update(dict(
        seed=seed,
        use_subtree_match_edges=use_subtree_match_edges,
        num_time_heads=num_time_heads,
        cost_norm=cost_normalizers.cost_normalizer(None),
        model_hypers=model_hypers,
    ))
    if sparse:
        return regressor.SparseGNNStateValueRegressor(**kwargs)
    else:
        return regressor.DenseGNNStateValueRegressor(**kwargs)


def test_save_restore_model():
    e = parse_expr_typed("(let ((a (add x 3.0))) (add (mul a a) y))")
    with tempfile.TemporaryDirectory() as tmpdir:
        npzfile = os.path.join(tmpdir, "test.npz")
        with get_test_regressor() as model_wrapper:
            # Random weights, but model deterministic given those weights
            res = model_wrapper.evaluate_all_time_left([e])
            weights = model_wrapper.get_weights()
            weights.save(npzfile)
        with get_test_regressor(seed=2) as model_wrapper2:
            assert (model_wrapper2.evaluate_all_time_left([e]) != res).any()
            model_wrapper2.load_weights(npzfile)
            np.testing.assert_equal(model_wrapper2.evaluate_all_time_left([e]), res)
        with get_test_regressor(seed=2) as model_wrapper3:
            assert (model_wrapper3.evaluate_all_time_left([e]) != res).any()
            model_wrapper3.set_weights(weights)
            np.testing.assert_equal(model_wrapper3.evaluate_all_time_left([e]), res)
        with get_test_regressor(hidden_dim=80) as incompatible_model_wrapper:
            with pytest.raises(Exception):
                incompatible_model_wrapper.load_weights(npzfile)
        with get_test_regressor(output_hidden_dim=100) as incompatible_model_wrapper:
            with pytest.raises(Exception):
                incompatible_model_wrapper.set_weights(weights)


@pytest.mark.parametrize("sparse", [False, True])
@pytest.mark.parametrize("num_gnn_blocks", [1, 5])
@pytest.mark.parametrize("message_from_sender_receiver", [True, False])
@pytest.mark.parametrize("aggregation_over_edge_types", ["sum", "concatenate", "concatenate_by_agg_types"])
def test_training_restored_model(
    sparse: bool,
    num_gnn_blocks: int,
    message_from_sender_receiver: bool,
    aggregation_over_edge_types: str,
):

    e = parse_expr_typed("(let ((a (div 1.0 x))) (div a (add 1.0 a)))")
    batch = [(e, range(10))]
    regr_params = {
        "recurrent_dropout":0.0,
        "output_dropout":0.0,
        "sparse":sparse,
        "num_gnn_blocks": num_gnn_blocks,
        "message_from_sender_receiver": message_from_sender_receiver,
        "aggregation_over_edge_types": aggregation_over_edge_types,
    }

    if num_gnn_blocks > 1 and not sparse:
        with pytest.raises(NotImplementedError) as e:
            get_test_regressor(**regr_params)
        assert e.value.args[0] == "StackedGNN is not implemented for DenseGNN."
        return
    if not sparse and message_from_sender_receiver:
        with pytest.raises(NotImplementedError) as e:
            get_test_regressor(**regr_params)
        assert e.value.args[0] == "Using receiver features in message computation is not implemented for DenseGNN."
        return

    with get_test_regressor(**regr_params) as model_wrapper:
        batch = model_wrapper.prepare_batch(batch)
        orig_weights = model_wrapper.get_weights()
        res1 = model_wrapper.evaluate_all_time_left([e])
        loss1 = model_wrapper.train_on_batch(batch)
        res2 = model_wrapper.evaluate_all_time_left([e])
        with pytest.raises(AssertionError):
            # Check that training actually did change the network
            np.testing.assert_equal(res1, res2)
        model_wrapper.set_weights(orig_weights)
        np.testing.assert_allclose(model_wrapper.evaluate_all_time_left([e]), res1, atol=1e-6)
        loss2 = model_wrapper.train_on_batch(batch)
        np.testing.assert_allclose(loss2, loss1, atol=1e-6)
        np.testing.assert_allclose(model_wrapper.evaluate_all_time_left([e]), res2, atol=1e-6)

@pytest.mark.parametrize("eager", [False, True])
def test_sparse_gnn_model(eager: bool):
    exprs = [
        parse_expr_typed("(let ((a (add x 3.0))) (add (mul a a) y))"),
        parse_expr_typed("(build m (lam (i : Integer) (build n (lam (j : Integer) (add (to_float i) (to_float j))))))", default_var_type = Type.Integer),
        parse_expr_typed("(let ((t (tuple x y z))) (add t t))")
    ]
    with get_test_regressor(sparse=False, eager=eager) as model_wrapper:
        expected_output = model_wrapper.evaluate_all_time_left(exprs)
        weights = model_wrapper.get_weights()
    with get_test_regressor(sparse=True, eager=eager) as model_wrapper:
        model_wrapper.set_weights(weights)
        output = model_wrapper.evaluate_all_time_left(exprs)
        np.testing.assert_allclose(expected_output, output, rtol=1e-4)


@pytest.mark.parametrize("sparse", [False, True])
@pytest.mark.parametrize("eager", [False, True])
def test_gnn_reproducible(sparse: bool, eager: bool):
    e = parse_expr_typed("(let ((a (div 1.0 x))) (div a (add 1.0 a)))")
    batch = [(e, range(10))]
    with get_test_regressor(output_dropout=0, sparse=sparse, eager=eager) as model_wrapper:
        prepped = model_wrapper.prepare_batch(batch)
        model_wrapper.train_on_batch(prepped)
        loss0 = model_wrapper.train_on_batch(prepped)
    with get_test_regressor(output_dropout=0, sparse=sparse, eager=eager) as model_wrapper:
        prepped = model_wrapper.prepare_batch(batch)
        model_wrapper.train_on_batch(prepped)
        loss1 = model_wrapper.train_on_batch(prepped)
    np.testing.assert_equal(loss0, loss1)

def test_evaluation_reproducible():
    e = parse_expr_typed("(let ((a (div 1.0 x))) (div a (add 1.0 a)))")
    with get_test_regressor() as model_wrapper:
        out1 = model_wrapper.evaluate_all_time_left([e])
        out2 = model_wrapper.evaluate_all_time_left([e])
    np.testing.assert_equal(out1, out2)

def test_cost_normalizers():
    import pickle
    costs = [5, 10, 100, 1000]
    values =  np.array([[0, 1, 2, 3, 4, 5],
                            [0, 1, 3, 9, 9.9, 10],
                            [0, 1, 2, 10, 99, 100],
                            [0, 1, 990, 998, 999, 999.9]])
    for name in cost_normalizers.available_cost_normalizers():
        normalizer = cost_normalizers.cost_normalizer(name)
        print(normalizer)
        norm_vals = np.array([[normalizer.normalize_value(adv, cost) for adv in advs] for advs, cost in zip(values, costs)])
        for cost, advs, vals in zip(costs, values, norm_vals):
            for adv, val in zip(advs, vals):
                print("Cost={}, value={}, normalized value={}".format(cost, adv, val))
        res = normalizer.denormalize_values(norm_vals, costs)
        np.testing.assert_allclose(res, values, rtol=1e-10, atol=1e-10)
        np.testing.assert_allclose(res, pickle.loads(pickle.dumps(normalizer)).denormalize_values(norm_vals, costs))
