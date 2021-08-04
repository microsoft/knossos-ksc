import os
import subprocess


def get_git_revision_short_hash():
    try:
        # pylint: disable=unexpected-keyword-arg # For some reason, pylint doesn't like "cwd"
        head_ref_names = (
            subprocess.check_output(
                ["git", "log", "--format=%D", "-1"], cwd=os.path.dirname(__file__)
            )
            .decode("ASCII")
            .strip()
        )
        if " -> " in head_ref_names:
            branch = "@" + head_ref_names.split(" -> ")[1].split(",")[0]
        else:
            branch = ""
        hash = (
            subprocess.check_output(
                ["git", "rev-parse", "--short", "HEAD"], cwd=os.path.dirname(__file__)
            )
            .decode("ASCII")
            .strip()
        )
        changed = (
            "+local_changes"
            if len(
                subprocess.check_output(
                    ["git", "diff-index", "HEAD", "--"], cwd=os.path.dirname(__file__)
                ).decode("ASCII")
            )
            > 0
            else ""
        )
        return "{}{}{}".format(hash, branch, changed)
    except (subprocess.CalledProcessError, FileNotFoundError):
        return "UNKNOWN GIT REVISION"
