# fmt: off
import json
import os
import pytest
import tempfile

from rlo.flags import get_scenario_name_and_defaults, make_config, run_arguments, config_arguments, parse_with_defaults

from testutils import scenario_path


def test_overriding():
    fewer_simplify_scenario = scenario_path('fewer_simplify_rules')
    config_argument_defaults = parse_with_defaults(config_arguments, {}, cmdline=[])
    run_argument_defaults = parse_with_defaults(run_arguments, {}, cmdline=[fewer_simplify_scenario])
    run_argument_defaults["scenario"] = "fewer_simplify_rules"

    scenario, defs = get_scenario_name_and_defaults(fewer_simplify_scenario)
    assert scenario == "fewer_simplify_rules"
    # Check a parameter not overridden by the scenario...
    assert defs['num_propagations'] == config_argument_defaults['num_propagations']
    # ...and a parameter that *is* overridden by the scenario
    assert defs['max_gnn_eval'] != config_argument_defaults['max_gnn_eval']
    # Specify a command-line override of a config_argument (max_epochs) and a run_argument (force_gpu)
    config = make_config(config_arguments + run_arguments, scenario, defs, cmdline=[fewer_simplify_scenario, "--max_epochs=9999", "--force_gpu", "--run_id=test1"])
    assert config['extra_scenario_params'] == "+max_epochs:9999" # Only records config_arguments
    assert config['force_gpu']
    assert config['run_id'] == "test1"
    for name, defval in run_argument_defaults.items():
        assert (name in ["force_gpu", "run_id"]) ^ (config[name] == defval)

    # Now use that for another scenario - this ideally would be another test, but we want to use the config created above
    with tempfile.TemporaryDirectory() as tmpdir:
        fname = os.path.join(tmpdir, "first.json")
        with open(fname, "w") as f:
            json.dump(config, f)

        scenario2, defs2 = get_scenario_name_and_defaults(fname)
        assert scenario2 == "fewer_simplify_rules" # Not first.json!
        assert all(defs2[k] == config[k] for k in config_argument_defaults.keys())
        assert set(defs2.keys()).isdisjoint(set(run_argument_defaults.keys()))
        config2 = make_config(config_arguments + run_arguments, scenario2, defs2, cmdline=[fname, "--max_gnn_eval=67"])
        assert config2['scenario'] == "fewer_simplify_rules"
        assert config2['extra_scenario_params'] == "+max_epochs:9999+max_gnn_eval:67"
        assert config2['max_gnn_eval'] == 67
        for name in config_argument_defaults.keys():
            assert (name == "max_gnn_eval") ^ (config2[name] == config[name])
        # ...but run arguments not inherited
        assert not config2['force_gpu']
        for name in run_argument_defaults.keys():
            assert (name in ["force_gpu", "run_id"]) ^ (config2[name] == config[name])

def test_legacy_config_rejected():
    path = os.path.join(os.path.dirname(__file__), "legacy_config.json")
    with open(path) as file:
        legacy_config = json.load(file)
    assert "num_timesteps" in legacy_config
    with pytest.raises(ValueError, match="num_timesteps has been replaced by num_propagations"):
        _scenario_name, _defs = get_scenario_name_and_defaults(path)
