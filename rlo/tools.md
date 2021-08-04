# Tools

`knnossos` has various continuous-integration (CI) tests in place to ensure code quality and correctness, including [formatting](./check_format.sh) and [pylint](./pylint.sh). The following tools may make it easier to comply with these checks locally before checking in code and failing the various CI tests. They are not automatically integrated into the workflow.

## VSCode

`knossos` uses [black](https://github.com/psf/black) for formatting. Note we currently use the default black line length of `88`. You may find the following settings useful. To use, copy them into `.vscode/settings.json` (don't commit them) or your user settings:

```json
    "python.formatting.provider": "black",
    "editor.formatOnSave": true,
    "editor.rulers": [
        88
    ],
```

## [Pre-commit](https://github.com/pre-commit)

Pre-commit is a package manager for various git hooks. It is configured in `.pre-commit-config.yaml`. The below configuration checks staged `json` and `yaml` files are valid, removes unused imports and variables (which would cause a `pylint` error otherwise), formats with `black` and runs `pylint` on the staged files. If any changes are made, you will need to re-stage the relevant files and try again.

To setup, run

```bash
pip install pre-commit
pre-commit install
```

You can apply the changes to all files using

```bash
pre-commit run --all-files
```

Usually these will only be run on staged files. The first run will take a bit of time, but subsequent runs should be relatively fast.
