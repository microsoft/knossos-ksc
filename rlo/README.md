[![Build Status](https://dev.azure.com/msrcambridge/Knossos/_apis/build/status/Build%20and%20Test?branchName=master)](https://dev.azure.com/msrcambridge/Knossos/_build/latest?definitionId=190?branchName=master)
# knossos
Knossos: The AI-driven AI Compiler

See OneNote: https://microsoft-my.sharepoint.com/personal/awf_microsoft_com/Documents/Notebooks/Knossos/

Knossos is a functional compiler and code-gen tool that will
accelerate writing AI algorithms as well as making them easier. The
code will be optimised using reinforcement learning, automating the
hand-optimization process that experts would spend a lot of time
doing. The output of code-gen is C++ code. (Note that RL piece is not
plugged into the compiler at the moment.) We envisage that Knossos
will have interfaces for popular languages such as Python, Julia and
F# which will make it easier for a wide variety of programmers to
leverage the benefits of Knossos.

The compiler project (ksc) has moved: https://github.com/microsoft/knossos-ksc

### RLO: The Reinforcement Learning Optimizer

RLO optimizes source code by applying rewrite rules, with the order of rule application being decided by an agent trained using reinforcement learning.

This code is based on open source [gated graph sequence neural network code](https://github.com/Microsoft/gated-graph-neural-network-samples).

#### Python GNN Agent (Linux)(under construction)
Installing with conda:

```bash
conda env create -f test/builds/conda-env.yaml -n knossos
conda activate knossos
pip install -r test/builds/ci-requirements.txt
```

Installing with pip (CPU only):

```
./test/builds/install_linux.sh
```

To commit and push changes to this repo, you will also need pre-commit:
```bash
pip install pre-commit
pre-commit install
```

The RLO code depends on the `ksc` submodule.  The submodule is at `knossos-ksc`. Code in `src/rlo/__init__.py` adds `knossos-ksc/src/python` to the path so that you can `import ksc`.  When you first clone the repo you must do this:
```
git submodule update --init --recursive
```


Usage examples:

```bash
python src/train_over_expressions.py simplify_rules --num_parallel NUM_GPUS
```
This will train an agent to optimize a set of expressions, 10 repetitions retraining from scratch, so takes an hour on a dual-GPU machine. Outputs .png plots. Shorter (minimal) example:

```bash
python src/train_over_expressions.py fewer_simplify_rules [--num_parallel NUM_GPUS]
```

#### Unit tests
Run fast tests:

```bash
pytest test/rlo/
```

Run all the tests (very slow!):

```bash
pytest test/rlo/ -s --runslow
```

### Testing and Graphs

Every commit gets checked for compilation and short-running tests as per the definition in [test/builds/quick_test.yml](test/builds/quick_test.yml). These builds appear as ['QuickTest'](https://dev.azure.com/msrcambridge/Knossos/_build?definitionId=193) on Azure DevOps.

Pull Requests are also checked for the installation process, and on all our supported platforms, as per the definition in [test/builds/build_and_test.yml](test/builds/build_and_test.yml). These builds appear as ['Build and Test'](https://dev.azure.com/msrcambridge/Knossos/_build?definitionId=190) on Azure DevOps. Only PRs that pass this test can be merged into master.

Any build can be queued manually by going to its Azure page and hitting the 'Queue' button. It asks for the branch (default is master) and the commit (default is HEAD).

For longer runs and graph production, the Azure Batch Graphs build below may be used, which is also run on the master branch every night if there has been a new commit.

#### Azure Batch graphs

RLO training demonstrates high variance, so generally we run RLO experiments 10 times over (the "num_repetitions") and plot confidence intervals across the repetitions. This takes a long time on one machine, but we have access to a pool of GPU machines which can run the repetitions in parallel. The ['Azure Batch Graphs'](https://msrcambridge.visualstudio.com/Knossos/_build?definitionId=257) allows to do this somewhat conveniently, by default running 10 repetitions of a couple of scenarios.

 After one of these builds completes, on the page for the particular run, use 'Artifacts' to view or download the graphs. Output from the "Azure CLI azure_batch.ps1" step displays the total amount of compute time taken and some command-lines to download the logs onto your own machine.

**Troubleshooting** is harder because the machines are only temporarily leased in a remote cluster, but some is possible using portal.azure.com. Under the Knossos subscription,
*  the "knossosbuildpipeline" Batch Account allows you to see machine/cluster utilisation; choose "Jobs", pick your job (named e.g. build-20190606-4) to see a list of tasks (each is a commandline executed on one machine), picking a task allows you to see e.g. start time, commandline, stdout.txt/stderr.txt on the node as in runs.
*  the "knossosbuildpipeline" Storage Account contains the results, uploaded as each task finishes. Go to "Blobs", "logs", then your build directory (e.g. build/20190606-4), then Run_xxx. Here the stdout/stderr from the plotting job is placed. In subdirectories 0/, 1/, etc., the stdout.txt + stderr.txt + events.json + events_all.json.

**Deciding what to run** - the Queue dialog, besides the branch/commit, also displays variables "num_repetitions", "days_to_live", and "SCENARIOS_FLAGS" - the values are editable (yes!). The last consists of flags (`--flag=value`) and one or more scenarios to run (e.g. `binding_simplify_astar`, `binding_generalization_36`, `generalization_blas`), which are passed through to all python jobs in the build. For example,
*  `fewer_simplify_rules` runs (num_repetitions) repetitions of the fewer_simplify_rules scenario for 2 generations, plus plotting job. This is about the smallest complete run.
*  `binding_simplify_8 binding_generalization_24 --train_search=rollout` runs repetitions of binding_simplify_8, plus plot, and the same number of repetitions of binding_generalization_24, plus plot, all with `--train_search=rollout` (overriding the scenario default)

#### Azure Batch Ray

Further to the above, another build is available that can distribute search+evaluation over multiple machines/GPUs (the same or different repetitions): [Azure Batch Ray](https://msrcambridge.visualstudio.com/Knossos/_build?definitionId=391). Here the number of nodes (machines) can be set independently from the number of repetitions (`--num_repetitions`, in the `SCENARIO_FLAGS` variable, or defaulting to the number of repetitions in the scenario: generally 10 but only 2 for some of the small/test scenarios). The `WORKERS_PER_NODE` field allows multiple threads to share the GPU on one machine, within memory limits; a value of 2 generally seems to work on the NC6s_v2's we use on Azure Batch. Note the scenario name (there may be only one) must be the first in the `SCENARIO_FLAGS` field, before any hyperparameter overrides such as `--num_repetitions`.

#### ADBench graphs

The ADBench graphs are available in the Artifacts section of each
build in the the [ADBench_on_selfhosted build
definition](https://msrcambridge.visualstudio.com/Knossos/_build?definitionId=205).
If you want to kick off your own build then click "Queue" and choose a
branch or commit.  At the time of writing the best thing to choose is
probably commit `e308fda1d7d6983dd16317456cbb8bb135db4889`.

In order to benchmark Knossos you need to use the [private ADBench
repository](https://msrcambridge.visualstudio.com/Knossos/_git/autodiff).
If you want to run ADBench locally with a different Knossos version
then copy the files `gmm.cpp`, `knossos.h` and `knossos.cpp` into
`tools/KnossosRev`.

### Developer Tools

See [./tools.md] for some tools which may aid your workflow when contributing to knossos.
