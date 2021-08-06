# fmt: off
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from typing import Sequence, Tuple

from rlo.expression import Expression, EF
from rlo import rewrites
from ksc.type import Type
from rlo.utils import all_distinct

_binding_sequences = None
def binding_sequences():
    """ Returns a list of 60 lists ("groups") of 6 lists ("sequences"),
        where each sequence starts with a different expression, then rewrites to optimal form in the shortest possible way.
        Within each group of 6, each sequence has a different "common" subexpression, which also appears unrepeated in one of the 6.
        The difficulty distribution (gain, number of steps) of each group is intended to be similar.
        We use the first two groups as test, some subset of the rest as train.
    """
    global _binding_sequences
    if _binding_sequences is None:
        x = Expression.Variable("x", type=Type.Float)
        y = Expression.Variable("y", type=Type.Float)

        sub_exps = [x + y, x * 2.0, x * y, x / (y - 1.0), 1.0/(x + 1.0), EF.Log(x)]
        # All the pairs of two distinct sub_exps. For each pair, we'll generate sequences
        # which repeat the first sub_exp, but contain the second sub_exp only once, hopefully
        # avoiding learning just to let-bind any expression in that set of sub_exps.
        diff_pairs = [[(e1,e2) for e2 in sub_exps if e1 != e2] for e1 in sub_exps]

        def combine(s, s2, lam):
            """ Given two sequences of expressions s1 and s2,
                    which should begin with no LETs and end with LET having been introduced,
                and a function combining two expressions,
                return a sequence of *combined* expressions,
                    which performs the steps in both of the input sequences,
                    followed by as few lift_bind<N> rewrites as possible in order to:
                    then terminates with a cse_bind rewrite. """
            steps = [lam(e,s2[0]) for e in s] + [lam(s[-1],e) for e in s2[1:]]
            while True:
                cse = [rewrite.apply_expr(steps[-1]) for rewrite in rewrites.rule("cse_bind").get_all_rewrites_expr(steps[-1])]
                assert len(cse) < 2
                if len(cse)>0:
                    steps.append(cse[0])
                    break
                steps.append(next(rewrite.apply_expr(steps[-1]) for rewrite in rewrites.rule("lift_bind").get_all_rewrites_expr(steps[-1])))
            return steps

        def arrangements(e, e2):
            """Given sub-expression e, which shall be duplicated, and e2, which will not,
            yields sequences each starting with an expression containing e and e2 and ending with all repeats CSE'd. """
            sing_var = [e, EF.Let(x, e,x)]
            # Embedded exprs, put e2 at outside so we don't need to lift past it - these are the shortest sequences.
            # (Must tack on e2 after call to combine to avoid lifting over it)
            yield [expr / e2 for expr in combine([2.0*e, 2.0*EF.Let(x, e,x)], sing_var, lambda x,y: x+y)] #6 steps
            yield [expr - e2 for expr in combine([2.0*e, 2.0*EF.Let(x, e,x)], sing_var, lambda x,y: x*y)] #6 steps
            yield [expr * e2 for expr in combine([2.0*e, 2.0*EF.Let(x, e,x)], sing_var, lambda x,y: x/(y - 1.0))] #6 steps
            # Also make exprs where e2 is deep inside the expr, and we must lift one of the "let"s over it
            for a,b in [([e + e2, EF.Let(x, e, x) + e2], [e * 3.0, EF.Let(x, e, x) * 3.0]), #7+x steps
                        ([e + 1.0, EF.Let(x, e, x) +  1.0], [e2 + e, e2 + EF.Let(x, e, x)])]: #7+x steps
                for lam in [lambda e,e2: e*e2, lambda e,e2: e+(e2 / 2.0), lambda e,e2: (e + 1.0)/(e2 - 1.0)]: #x=0, +1, +2
                    yield combine(a, b, lam)

            # Now some 3-occurrence examples. Do this carefully to ensure steps are few enough.
            # In particular we must combine the two furthest-down and closest-together occurrences first:
            a = list(combine(sing_var, sing_var, lambda x,y: x*y))
            # Then combine those two with the third occurrence:
            yield combine(a, sing_var, lambda x,y: x/(y-e2))

            a = list(combine(sing_var, sing_var, lambda x,y: x/(e2+y))) #again, the deepest+closest first
            for lam in [lambda x,y: x+y, lambda x,y: x*y]:
                yield combine(a, sing_var, lam)

        # Make an array where the first index selects a sub_exp to be repeated
        #    (this determines the total gain/value for 2- or 3- occurrences);
        # the second index selects a different sub_exp that will appear unduplicated
        #   (note that the second index being the same, means the unduplicated sub_exp will be the same, IF the first index is also the same);
        # the third index selects one of the arrangements
        #   (determining #steps-required, and whether there are 2 or 3 occurrences).
        extra_exps = np.array([[list(arrangements(e1,e2)) for e1,e2 in lst] for lst in diff_pairs])
        assert extra_exps.shape == (6, 5, 12)

        # We'll make NUM_GROUPS groups of GROUP_SIZE expressions each. Let s be the group (in range(NUM_GROUPS)),
        # and i the index within (in range(GROUP_SIZE)); return the 3-dimensional index into extra_exps
        NUM_GROUPS = 60
        GROUP_SIZE = 6
        def index_func(i, s):
            return ( i, (i + s*3) % extra_exps.shape[1], (i*5 + s) % extra_exps.shape[2])
        # Between all the groups, this should cover every sequence in extra_exps exactly once.
        assert NUM_GROUPS * GROUP_SIZE == extra_exps.size
        assert len(set([index_func(i, s) for i in range(GROUP_SIZE) for s in range(NUM_GROUPS)])) == extra_exps.size

        # Within each group, we want each expression to have a different repeated sub_exp
        # (first index) - this is trivial by the first component of index_func above
        assert all([ all_distinct([index_func(i,s)[0] for i in range(GROUP_SIZE)]) for s in range(NUM_GROUPS)])
        # We also want each of the 6 expressions to have a different third index (arrangement)
        assert all([ all_distinct([index_func(i,s)[2] for i in range(GROUP_SIZE)]) for s in range(NUM_GROUPS)])

        # Finally we want each to have a different unrepeated expression.
        # This is not the same as the second index being distinct (as the second dimension is smaller,
        # the repeated subexpression already having been discounted)
        def unrepeated(i, s):
            fst, snd, _ = index_func(i, s)
            _, unc = diff_pairs[fst][snd]
            return unc
        assert all([ all_distinct([unrepeated(i,s) for i in range(GROUP_SIZE)]) for s in range(NUM_GROUPS)])
        _binding_sequences = [
            [extra_exps[index_func(i,s)] for i in range(GROUP_SIZE)] for s in range(NUM_GROUPS)
        ]
    return _binding_sequences

def get_bindgen_train_sequences(num_train_exprs):
    assert (num_train_exprs % 6) == 0
    num_train_sets = num_train_exprs // 6
    all_sets = binding_sequences()
    return [seq for seqs in all_sets[2:num_train_sets+2] for seq in seqs]

def get_bindgen_test_sequences():
    all_sets = binding_sequences()
    return all_sets[0] + all_sets[1]

def binding_examples(num_train_exprs=None, outfile=None, verbose = None) -> Tuple[Sequence[Expression], Sequence[Expression]]:
    """
        Returns a pair of (train exprs, eval exprs).
        The 12 test exprs are fixed; the train exprs must be a multiple of 6, or None to use all available (348).
        If outfile is non-None, a plot of absolute/relative gain vs. num-steps is produced
          of the test, train, omitted, and oracle expression (sequences). """

    all_sets = binding_sequences()
    if num_train_exprs is None:
        num_train_sets = len(all_sets) - 2
    elif (num_train_exprs % 6) == 0 and num_train_exprs >= 6 and (num_train_exprs//6 + 2) <= len(all_sets):
        num_train_sets = num_train_exprs//6
    else:
        raise ValueError("Must have a multiple of 6 training exprs, between 6 and {}".format(
            6*(len(all_sets)-2)))

    eval_seqs = all_sets[0] + all_sets[1]
    train_seqs = [seq for seqs in all_sets[2:num_train_sets+2] for seq in seqs]

    if outfile is not None:
        from rlo import best_results
        from rlo.expr_sets import original_binding_expressions
        oracle_seqs = [best_results.oracle_sequence(exp, "binding_rules") for exp in original_binding_expressions]

        _, axs = plt.subplots(1, 2, figsize=(12, 10))

        def rand_jitter(arr):
            stdev = .05*(max(arr)-min(arr))
            return arr + np.random.randn(len(arr)) * stdev

        def plot(seqs, label, color, alpha):
            steps = rand_jitter([len(seq)-1 for seq in seqs])
            axs[0].scatter(steps, rand_jitter([seq[0].cost() - seq[-1].cost() for seq in seqs]), color=color, label=label, alpha=alpha)
            axs[1].scatter(steps, rand_jitter([(seq[0].cost() - seq[-1].cost())/seq[0].cost() for seq in seqs]), color=color, label=label, alpha=alpha)

        cmap=plt.get_cmap("plasma")
        plot(eval_seqs, "test", "green", alpha=0.8)
        plot(train_seqs, "train", "blue", alpha=0.8)
        plot(oracle_seqs, "oracle", "red", alpha=0.8)
        for i, seqs in enumerate(all_sets[num_train_sets+2:]):
            plot(seqs, None, cmap((i+num_train_sets+2) / len(all_sets)), alpha=0.25)

        plt.figlegend(*axs[1].get_legend_handles_labels(), loc="lower center")
        plt.xlabel("Steps required", fontsize=16)
        axs[0].set_ylabel("Absolute gain", fontsize=16)
        axs[1].set_ylabel("Relative gain", fontsize=16)
        plt.savefig(outfile)
    if verbose:
        for title, seqs in [("TRAINING SET", train_seqs), ("EVAL SET", eval_seqs)]:
            print(title)
            for seq in seqs:
                exp = seq[0]
                gain = round(exp.cost() - seq[-1].cost(), 2)
                nsteps = len(seq)-1
                diff = nsteps/gain
                rel_gain = gain / exp.cost()
                print("Difficulty {} ({} steps for {} = {}%): {}".format(diff, nsteps, gain, round(rel_gain*100), exp))
    return ([seq[0] for seq in train_seqs], [seq[0] for seq in eval_seqs])
