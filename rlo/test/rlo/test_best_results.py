# fmt: off
import functools
import itertools
from numbers import Number
import pytest
import os

from rlo.expression import Expression, EF
from rlo import best_results
from rlo import expr_sets
from rlo import rewrites
from rlo import utils
from ksc.type import Type

# Importing prettyprint to get the decorated printers for Expression and Type
from rlo.prettyprint import cpprint

# This tests that the expressions in best_results can be reached by following the intermediate/proof steps
def test_best_results_steps():
    for rules_name,exprs in best_results._best_results.items():
        rules = rewrites.get_rules(rules_name)
        for steps, _ in exprs:
            current_expression = steps[0]
            for n in steps[1:]:
                candidates = [rewrite.apply(current_expression) for rewrite in rules.get_all_rewrites(current_expression)]
                if n not in candidates:
                    print("Sequence start: {}".format(steps[0]))
                    print("Previous step:  {}".format(current_expression))
                    print("Could not find: {}".format(n))
                    for cand in candidates:
                        print("** Candidate:   {}".format(cand))
                    assert False
                current_expression = n

# Checks that no lower-cost expression than that in best_results can be reached in that number of steps,
# and that an equal cost cannot be reached in fewer steps.
def brute_force_test_best_results(skip_if_too_big):
    for rules_name, exprs in best_results._best_results.items():
        rules = rewrites.get_rules(rules_name)
        for best_seq, check_type in exprs:
            start = best_seq[0]
            if isinstance(check_type, Number):
                cost_limit = start.cost() + check_type # Also means, allow "quick" test to bail
            else:
                assert check_type in ["optimal", "steps"]
                cost_limit = None # Do not allow "quick" test to bail

            best_cost = best_seq[-1].cost()
            num_steps = len(best_seq) - 1 #Excluding starting expr
            print("\nStarting {} initial cost={} target cost={} in {} steps".format(
                rules_name, start.cost(), best_cost, num_steps))
            cpprint(start.expr)
            all_exprs_seen = {start}
            cands = [(start, [])] # List of (Expression, steps = list of (node_index, rule, result_expr))
            for s in itertools.count() if check_type == "optimal" else range(num_steps):
                print("{} steps/{}, {} candidates".format(s, num_steps, len(cands)))
                if skip_if_too_big:
                    if (cost_limit is not None) and (len(cands)*(num_steps-s) > 1500):
                        print("Skipping as too long")
                        break
                next_cands = []
                for cand,steps in cands:
                    for rewrite in rules.get_all_rewrites(cand):
                        next_expr = rewrite.apply(cand)
                        if next_expr in all_exprs_seen: continue
                        if (cost_limit is not None) and next_expr.cost() > cost_limit: continue
                        all_exprs_seen.add(next_expr)
                        next_cands.append((next_expr, steps + [("{}@{}".format(rewrite.rule_name, rewrite.node_id), next_expr)]))
                if len(next_cands) == 0:
                    assert (s == num_steps) or (check_type == "optimal" and s > num_steps) or (cost_limit is not None)
                    # Exhaustive (/cost-limited) search _to any depth_ complete
                    break
                min_cost = min(e.cost() for e,_ in next_cands)
                # Check we haven't "beaten" the oracle sequence.
                # Don't check we have matched the oracle sequence - that's done in test_best_results_steps above.
                if (min_cost < best_cost or (s < (num_steps - 1) and min_cost == best_cost)):
                    e, steps = next((e,steps) for (e,steps) in next_cands if e.cost() == min_cost)
                    msg = f"cost {min_cost} < {best_cost} or steps {s} < {num_steps-1} for expr {e.expr}"
                    print(f"ERROR: {msg}")
                    raise ValueError(f"Beat oracle cost for {start.expr}: {msg}")
                cands = next_cands

@pytest.mark.slow
def test_best_results_are_best():
    brute_force_test_best_results(False)

@pytest.mark.notquick
def test_best_results_not_easily_beaten():
    brute_force_test_best_results(True)

def test_best_results_keys():
    assert set(best_results._best_results.keys()) == set(rewrites.available_rules())

def test_best_results_alpha_conversion():
    # Check that best results entries are reachable from S-Expression strings that are alpha-convertible (not equal)
    # to those in best_results.py.  Possibly a bit redundant now?
    x = Expression.Variable("x", Type.Float)
    a = Expression.Variable("a", Type.Float)
    e = EF.Let(a, 1.0 / x, a / (1.0 + a))
    # Check the expression is found in the dictionary in all three representations
    assert best_results.best_cost_for_exp(e, "simplify_rules").cost > 0.0
    assert len(best_results.oracle_sequence(e, "simplify_rules")) > 0

    s_exp = "(let a (div 1.0 x) (div a (add 1.0 a)))"
    assert best_results.best_cost_for_exp(s_exp, "simplify_rules").cost > 0.0
    assert len(best_results.oracle_sequence(s_exp, "simplify_rules")) > 0

    v = Expression.Variable("crazy_variable_name")
    e2 = EF.Let(v, 1.0 /x, v / (1.0 + v))
    # We should be able to find the best cost for that expression (as its alpha-convertible)
    assert best_results.best_cost_for_exp(e2, "simplify_rules").cost > 0.0
    assert len(best_results.oracle_sequence(e2, "simplify_rules")) > 0
    # ...and also for the s-expression form, as we can parse that to an Expression object and test alpha-conversion:
    assert best_results.best_cost_for_exp("(let cv (div 1.0 x) (div cv (add 1.0 cv)))", "simplify_rules").cost > 0.0
    assert len(best_results.oracle_sequence("(let cv (div 1.0 x) (div cv (add 1.0 cv)))", "simplify_rules")) > 0

@pytest.mark.parametrize("file", ["ksc/blas/blas_combined.kso", "ksc/blas/blas_test.kso"])
@pytest.mark.parametrize("rules_name", ["ml_rules_no_bind"])
@pytest.mark.parametrize("max_time_left", [100])
def test_best_cost_for_expression(file, rules_name, max_time_left):
    expr_set = expr_sets.get_expression_set(file)
    for expr_name, _expr in expr_set.named_exprenvs():
        best_costs = [expr_set.best_cost_for_expression(expr_name, rules_name, i) for i in range(max_time_left + 1)]
        assert all(best_costs[i].cost >= best_costs[i+1].cost for i in range(max_time_left))
        assert all(len(b.sequence) <= i for i, b in enumerate(best_costs))

@pytest.mark.parametrize(
    "file",
    ["ksc/blas/blas_combined.kso", "ksc/blas/blas_test.kso", "ksc/blas/blas_train.kso", "ksc/release/gelu.kso"]
)
def test_recorded_sequences_possible(file):
    expr_set = expr_sets.get_expression_set_from_ks(file)
    @functools.lru_cache(maxsize=len(rewrites.available_rules()))
    def names_in_ruleset(rules_name):
        return [r.name for r in rewrites.get_rules(rules_name)]
    exprs = dict(expr_set.named_exprenvs())
    for entry in utils.parse_best_episodes(expr_set._file_path):
        rewrites.rewrite_seq_to_exprenvs(exprs[entry.func_name], entry.seq, strict=True)
        for _node_id, rule_name in entry.seq:
            assert rule_name in names_in_ruleset(entry.rules_name)

def test_best_costs():
    expr_name = "expr"
    max_time_left = 11
    rules_name = "binding_simplify_rules"
    file_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "test_sparser_files/best_costs.kso")
    expr_set = expr_sets.get_expression_set(file_path)
    best_costs = [expr_set.best_cost_for_expression(expr_name, rules_name, i).cost for i in range(max_time_left + 1)]
    assert best_costs == [5.1, 4.1, 3.1, 3.1, 3.1, 3, 3, 3, 2.1, 2.1, 2, 2]

@pytest.mark.parametrize("file", ["ksc/blas/blas_combined.kso", "ksc/blas/blas_test.kso", "ksc/blas/blas_train.kso"])
def test_best_costs_have_configs(file):
    folder = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../../src/rlo/")
    saved_configs = os.listdir(os.path.join(folder, "ksc/configs/"))
    file_path = os.path.join(folder, file)
    for best_ep_entries in utils.parse_best_episodes(file_path):
        if best_ep_entries.rlo_or_expert == "rlo": # only rlo source has config files
            assert f"{best_ep_entries.config_or_git_id}.json" in saved_configs
