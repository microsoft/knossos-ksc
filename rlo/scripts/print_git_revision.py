import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__), "../src"))
from rlo import git_utils

print(git_utils.get_git_revision_short_hash())
