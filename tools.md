# Tools

`knossos` has various continuous-integration (CI) tests in place to ensure code quality and correctness, including formatting. The following tools may make it easier to comply with these checks locally before checking in code and failing the various CI tests. They are not automatically integrated into the workflow.

## VSCode

`knossos` uses [black](https://github.com/psf/black) for formatting. Note we currently use the non-default black line length see [pyproject.toml](pyproject.toml) . You may find the following settings useful. To use, copy them into `.vscode/settings.json` (don't commit them) or your user settings:

```json
    "python.formatting.provider": "black",
    "editor.formatOnSave": true,
```

## [Pre-commit](https://github.com/pre-commit)

Pre-commit is a package manager for various git hooks that process files and can help with consistency and correctness. It is configured in [.pre-commit-config.yaml](.pre-commit-config.yaml) . If any changes are made, you will need to re-stage the relevant files and try again.

Per-user, to setup, run

```bash
pip install --requirement .\requirements-dev.txt
pre-commit install
```

Usually pre-commit will only be run on staged files. The first run will take a bit of time, but subsequent runs should be relatively fast.

You can apply pre-commit and hooks to all files using

```bash
pre-commit run --all-files
```
