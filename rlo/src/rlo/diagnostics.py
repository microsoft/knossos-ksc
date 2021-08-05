import os


def main():
    show_versions()
    show_env_vars()


def show_versions():
    commands = [
        "which python",
        "which python3",
        "python --version",
        "python3 --version",
        "python -m pip list",
        "python3 -m pip list",
        "nvidia-smi",
        "lsb_release -a",
    ]

    for command in commands:
        os.system(command)


def show_env_vars():
    # For a list of all environment variables supported by Azure Batch
    # see
    #
    # https://docs.microsoft.com/en-us/azure/batch/batch-compute-node-environment-variables
    env_vars = [
        "AZ_BATCH_ACCOUNT_URL",
        "AZ_BATCH_JOB_ID",
        "AZ_BATCH_POOL_ID",
        "AZ_BATCH_NODE_ID",
        "AZ_BATCH_NODE_IS_DEDICATED",
        "AZ_BATCH_TASK_ID",
    ]

    for env_var in env_vars:
        value = os.environ.get(env_var)
        print("%s: %s" % (env_var, value))


if __name__ == "__main__":
    main()
