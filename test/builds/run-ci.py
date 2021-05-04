# Dry-run the Azure Pipelines CI build

"""
run-ci azure-pipeline-spec.yml TAG 

Will find azure pipeline specs whose displayName contains [TAG], 
and emit a shell script to run those.  Expected usage:

python test/builds/run-ci.py test/builds/build_and_test.yml userInstall | sh -x # Run once

python test/builds/run-ci.py test/builds/build_and_test.yml userTest | sh -x # Run to check CI tests

TODO: variables etc. 

"""

import yaml
import platform
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("filename", default="test/builds/build_and_test.yml", nargs="?")
parser.add_argument("tag", default="userTest", nargs="?")
args = parser.parse_args()

with open(args.filename) as f:
    y = yaml.load(f, Loader=yaml.SafeLoader)

# from prettyprinter import cpprint
# cpprint(y)


def dosteps(steps):
    for step in steps:
        if args.tag in step.get("displayName", ""):
            if "script" in step:
                print("# ", step.get("displayName", "DISPLAYNAME NOT SET"))
                print(step["script"])
            else:
                print("# unrecognised step type: ", step)


if "steps" in y:
    dosteps(y["steps"])

if "jobs" in y:
    jobtag = {"Windows": "Windows", "Linux": "Ubuntu"}[platform.system()]

    for job in y["jobs"]:
        if job["job"] == jobtag:
            print("#JOB: ", jobtag)
            dosteps(job["steps"])
