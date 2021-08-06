import argparse
import base64
import io
import numpy as np
import matplotlib.pyplot as plt
from textwrap import wrap

from rlo import experiment_result
from rlo.experiment_result import AzureBlobExperimentResult
from rlo import factory
from rlo import plotting
from rlo import plot_costs
from rlo import utils


def calculate_summary(df_events):
    # pylint: disable=singleton-comparison
    num_exprs_train = df_events[df_events["is_train"] == True]["expr"].nunique()
    # pylint: disable=singleton-comparison
    num_exprs_test = df_events[df_events["is_train"] == False]["expr"].nunique()
    expressions_dict = {"train": num_exprs_train, "test": num_exprs_test}

    df = (
        df_events.groupby(["repetition", "expr", "generation"])
        .agg(
            almost_solved_train=("almost_solved_train", any),
            almost_solved_test=("almost_solved_test", any),
            is_train=("is_train", any),
        )
        .groupby(["repetition", "generation"])
        .agg(
            num_almost_solved_train=("almost_solved_train", "sum"),
            num_almost_solved_test=("almost_solved_test", "sum"),
            expr_train=("is_train", lambda x: x.eq(True).sum()),
            expr_test=("is_train", lambda x: x.eq(False).sum()),
        )
    )

    #  Some repetitions can have missing logs, and hence for example smaller number of train/test expressions.
    #  If this happens, set fraction of solved expressions to Nan, so that it does not participate in the computations.
    #  If not set to Nan, it would lead to skewed performance results.
    df["fraction_almost_solved_train"] = df.apply(
        (
            lambda x: (
                np.float("NaN")
                if x["expr_train"] != num_exprs_train
                else x["num_almost_solved_train"] / num_exprs_train
                if num_exprs_train != 0
                else 0
            )
        ),
        axis=1,
    )
    df["fraction_almost_solved_test"] = df.apply(
        (
            lambda x: (
                np.float("NaN")
                if x["expr_test"] != num_exprs_test
                else x["num_almost_solved_test"] / num_exprs_test
                if num_exprs_test != 0
                else 0
            )
        ),
        axis=1,
    )
    df["full_repetition"] = df.apply(
        lambda x: not (
            np.isnan(x["fraction_almost_solved_train"])
            or np.isnan(x["fraction_almost_solved_test"])
        ),
        axis=1,
    )

    df2 = df.groupby(["generation"]).agg(
        num_full_repetitions=("full_repetition", lambda x: x.eq(True).sum()),
        num_almost_solved_train_max=("num_almost_solved_train", "max"),
        num_almost_solved_test_max=("num_almost_solved_test", "max"),
        num_almost_solved_train_mean=("num_almost_solved_train", "mean"),
        num_almost_solved_test_mean=("num_almost_solved_test", "mean"),
        num_almost_solved_train_std_dev=("num_almost_solved_train", "std"),
        num_almost_solved_test_std_dev=("num_almost_solved_test", "std"),
        fraction_almost_solved_train_max=("fraction_almost_solved_train", "max"),
        fraction_almost_solved_test_max=("fraction_almost_solved_test", "max"),
        fraction_almost_solved_train_mean=("fraction_almost_solved_train", "mean"),
        fraction_almost_solved_test_mean=("fraction_almost_solved_test", "mean"),
        fraction_almost_solved_train_std_dev=("fraction_almost_solved_train", "std"),
        fraction_almost_solved_test_std_dev=("fraction_almost_solved_test", "std"),
    )

    return df2, expressions_dict


def plot_solved_graph(df2, expressions_dict, filename, graph_title):
    # This is how we get the "generation" column. It's bizarre, but
    # that's pandas for you.
    generation = df2.index.get_level_values(0).tolist()

    fracs_almost_solved = {
        "train": (
            df2["fraction_almost_solved_train_mean"],
            df2["fraction_almost_solved_train_std_dev"],
            df2["fraction_almost_solved_train_max"],
            "blue",
        ),
        "test": (
            df2["fraction_almost_solved_test_mean"],
            df2["fraction_almost_solved_test_std_dev"],
            df2["fraction_almost_solved_test_max"],
            "orange",
        ),
    }

    non_zero_keys = [k for k, v in expressions_dict.items() if v != 0]
    _, ax1 = plt.subplots(figsize=[10, 6])

    for k in non_zero_keys:
        (
            frac_almost_solved,
            frac_almost_solved_stddev,
            frac_almost_solved_max,
            color,
        ) = fracs_almost_solved[k]
        ax1.errorbar(
            x=generation,
            y=frac_almost_solved,
            yerr=frac_almost_solved_stddev,
            fmt="o",
            label=k + ": at least 95% of optimal (mean and std dev)",
            color=color,
        )
        ax1.errorbar(
            x=generation,
            y=frac_almost_solved_max,
            fmt="x",
            label=k + ": at least 95% of optimal (maximum)",
            color=color,
        )

    ax1.set_ylim(-0.2, 1)
    ax1.set_ylabel("Proportion of expressions across repetitions")
    ax1.set_xlabel("Generation (0 is untrained)")

    bar = ax1.scatter(
        generation,
        [-0.1] * len(generation),
        c=df2["num_full_repetitions"],
        cmap=plt.get_cmap("gist_rainbow"),
        marker="s",
        label="num_repetitions",
        s=100,
    )
    plt.colorbar(bar)
    plt.title(graph_title)

    plt.locator_params(nbins=4)
    plt.legend(loc="upper center", bbox_to_anchor=(0.5, -0.2))
    plt.tight_layout(rect=[0, 0, 1, 0.95])
    plt.savefig(filename)
    plt.close()


def get_graph_title(config, events):
    expression_summary = next(e for e in events if e["event"] == "expression_summary")
    num_train_expr = expression_summary["num_train_expr"]
    num_test_expr = expression_summary["num_test_expr"]

    (
        average_gen_train_time,
        average_total_train_time,
    ) = average_train_times_from_events(events)

    train_test_exprs = f"{num_train_expr} train and {num_test_expr} test expressions"
    train_time = f"average total train time = {average_total_train_time}, average train time per gen = {average_gen_train_time}"

    title_prefix = "\n".join(wrap(plotting.config_suffix(config), 80))
    graph_title = f"""{title_prefix},
        {train_test_exprs},
        {train_time}"""

    return graph_title


def plot_experiment_stats(config, events, graph_filename):

    graph_title = get_graph_title(config, events)
    best_known_cost = factory.best_cost_from_config(config)

    df_events = plot_costs.make_dataframe(events)
    df_events = plot_costs.compute_optimality(df_events, best_cost_fn=best_known_cost)

    almost_solved_threshold = 0.95

    df_events["almost_solved_train"] = df_events.apply(
        lambda x: x["optimality"] >= almost_solved_threshold and x["is_train"], axis=1,
    )
    df_events["almost_solved_test"] = df_events.apply(
        lambda x: x["optimality"] >= almost_solved_threshold and not x["is_train"],
        axis=1,
    )

    if len(df_events) == 0:
        raise ValueError(
            "There were no events! Without any events, plotting doesn't work properly."
        )
    else:
        df2, expressions_dict = calculate_summary(df_events)
        plot_solved_graph(
            df2, expressions_dict, graph_filename, graph_title,
        )


def img_of_bytes(bytes_):
    string_ = str(base64.b64encode(bytes_), "ascii")
    return '<img src="data:image/png;base64,%s">' % string_


def average_train_times_from_events(events):
    events_by_rep = [
        e
        for e in events
        if e["event"] == "expr_value_info" and "repetition" in e and "generation" in e
    ]

    generations = 0
    total_train_time = 0
    by_reps = utils.group_by(events_by_rep, lambda r: r["repetition"])

    for rep_events in by_reps.values():
        max_event = max(rep_events, key=lambda e: e["generation"])
        total_train_time += max_event["total_train_time"]
        generations += max_event["generation"]

    return (
        round(total_train_time / generations, 1),
        round(total_train_time / len(by_reps), 1),
    )


def plot_from_experiments_dict(experiments):
    all_search_algs = ["astar", "beam", "rollout", "hybrid", "2vf"]

    def experiment_graph(search_alg, experiment_id):
        assert search_alg in all_search_algs

        config, events = experiment_id.config_and_events()
        bytesio = io.BytesIO()

        plot_experiment_stats(config, events, bytesio)

        return bytesio.getvalue()

    experiment_graphs = dict(
        (
            scenario,
            {
                search_alg: experiment_graph(search_alg, experiment_id)
                for (search_alg, experiment_id) in search_algs.items()
            },
        )
        for (scenario, search_algs) in experiments.items()
    )

    print("<table>")
    print("<tr>")
    print("<th>Scenario</th>")
    for search_alg in all_search_algs:
        print("<th>%s</th>" % search_alg)
    print("</tr>")

    for (scenario, search_algd) in experiment_graphs.items():
        print("<tr>")
        print("<td>%s</td>" % scenario)
        for search_alg in all_search_algs:
            img_bytes = search_algd.get(search_alg)

            cell_contents = (
                img_of_bytes(img_bytes)
                if img_bytes is not None
                else "<p>No experiment</p>"
            )

            print("<td>%s</td>" % cell_contents)

        print("<tr>")


def main():
    built_in = {
        "binding_simplify": {
            "astar": from_knossosbuildpipeline("binding_simplify_astar_20201215-7"),
            "rollout": from_knossosbuildpipeline("binding_simplify_astar_20210105-6"),
            "beam": from_knossosbuildpipeline("binding_simplify_astar_20201215-11"),
            "hybrid": from_knossosbuildpipeline("binding_simplify_astar_20210104-2"),
            "2vf": from_knossosbuildpipeline("binding_simplify_astar_20210114-6"),
        },
        "binding_generalization": {
            "astar": from_knossosbuildpipeline("binding_generalization_24_20201216-5"),
            "rollout": from_knossosbuildpipeline(
                "binding_generalization_24_20210105-12",
            ),
            "beam": from_knossosbuildpipeline("binding_generalization_24_20201215-12"),
            "hybrid": from_knossosbuildpipeline("binding_generalization_24_20210104-4"),
            "2vf": from_knossosbuildpipeline("binding_generalization_24_20210114-5"),
        },
        "summations": {
            "astar": from_knossosbuildpipeline("summations_20210105-9"),
            "rollout": from_knossosbuildpipeline("summations_20210105-11"),
            "beam": from_knossosbuildpipeline("summations_20201215-15"),
            "hybrid": from_knossosbuildpipeline("summations_20201215-16"),
            "2vf": from_knossosbuildpipeline("summations_20210108-4"),
        },
        "summations20": {
            "astar": from_knossosws(
                "52e580f9-8a35-4130-aebd-dce3bb0176b4",
                run_id="2021-01-11-15-05-12_summations20",
            ),
            "rollout": from_knossosws(
                "3d2cc45c-b84a-4bef-9b28-c29e6244d4f8",
                run_id="2021-01-12-11-13-28_summations20",
            ),
            "beam": from_knossosws(
                "fad05394-4bbe-4cf2-8946-36f7f87f4425",
                run_id="2021-04-15-09-22-31_summations20",
            ),
            "hybrid": from_knossosws(
                "fe5d8a89-6e6d-4b07-9fdc-5ff06f75b521",
                run_id="2021-01-11-15-00-59_summations20",
            ),
            "2vf": from_knossosws(
                "7114eaf4-e746-4ddb-8ad6-65a05d73b870",
                run_id="2021-01-14-20-35-28_summations20",
            ),
        },
        "blas_combined": {
            "astar": from_knossosbuildpipeline("blas_combined_20210111-5"),
            "rollout": from_knossosws(
                "blas_combined_1610539392_6580229b_head",
                run_id="2021-01-13-12-01-39_blas_combined",
            ),
            "beam": from_knossosbuildpipeline("blas_combined_20210112-5"),
            "hybrid": from_knossosbuildpipeline("blas_combined_20210112-7"),
            "2vf": from_knossosws(
                "2b35d890-4c68-42d4-a22e-e50f852cc20d",
                run_id="2021-01-14-10-40-43_blas_combined",
            ),
        },
        "blas": {
            "astar": from_knossosws(
                "3a988720-6f4c-4970-ad87-0f9950b02782",
                run_id="2021-01-13-11-30-24_blas",
            ),
            "beam": from_knossosws(
                "be89c089-bdc4-410d-9703-7437c25c27f1",
                run_id="2021-01-11-14-36-15_blas",
            ),
            "hybrid": from_knossosws(
                "40e124ed-82b7-42b8-8542-ba7bbdf26186",
                run_id="2021-01-11-15-34-51_blas",
            ),
            "2vf": from_knossosws(
                "5cd14f65-2ea5-498d-b5f7-b806732f9c56",
                run_id="2021-01-15-11-33-31_blas",
            ),
        },
        "blas_bind": {
            "astar": from_knossosws(
                "1a115822-65d9-4167-95dc-2a6c145ed6f7",
                run_id="2021-02-05-11-14-08_blas_bind",
            ),
            "beam": from_knossosws(
                "aa8067c4-9a88-4fb5-8d1a-b366702415a3",
                run_id="2021-02-05-11-22-17_blas_bind",
            ),
            "hybrid": from_knossosws(
                "9b4da2c0-a2e8-43d0-82a4-91ac97087833",
                run_id="2021-02-05-11-26-40_blas_bind",
            ),
            "2vf": from_knossosws(
                "f57b8521-3be2-4893-9200-2f181a896838",
                run_id="2021-02-05-11-30-41_blas_bind",
            ),
        },
    }

    plot_from_experiments_dict(built_in)


def from_knossosbuildpipeline(run_id):
    return AzureBlobExperimentResult.from_knossosbuildpipeline(run_id)


def from_knossosws(azureml_run_id, run_id=None):
    result = AzureBlobExperimentResult.from_azureml("knossosws", azureml_run_id, run_id)
    if run_id is not None:
        assert result.run_id() == run_id
    return result


def plot_success_summary_from_config(config, events):
    outfile = plotting.format_figure_filename(config, "success_summary.png")
    plot_experiment_stats(config, events, outfile)


def plot_from_local_files(run_id):
    config, events = experiment_result.load_config_events_locally(run_id)
    plot_success_summary_from_config(config, events)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--run_id", help="Run ID")
    args = parser.parse_args()
    if args.run_id is not None:
        plot_from_local_files(args.run_id)
    else:
        main()
