from rlo.plot_summations_fit_animated import (
    plot_summations_fit_animated_from_config,
    plot_summations_expert_fit_from_config,
)


def plot_name(fn_name):
    prefix = "plot_"
    suffix = "_from_config"
    assert fn_name.startswith(prefix)
    assert fn_name.endswith(suffix)
    name = fn_name[len(prefix) :]
    return name[: len(name) - len(suffix)]


extra_plots = {
    plot_name(fn.__name__): fn
    for fn in [
        plot_summations_fit_animated_from_config,
        plot_summations_expert_fit_from_config,
    ]
}
