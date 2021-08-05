import os
from yaml import safe_load


def main():
    """ Print out the pip packages, excluding those that are marked `skip-if-cpu` """
    with open(os.path.join("test", "builds", "conda-env.yaml")) as f:
        all_lines = f.read().split("\n")

    lines = [x for x in all_lines if "skip-if-cpu" not in x]
    doc = safe_load("\n".join(lines))

    pip_items = [
        item for item in doc["dependencies"] if isinstance(item, dict) and "pip" in item
    ]
    assert len(pip_items) == 1
    item = pip_items[0]
    assert list(item.keys()) == ["pip"]
    for pip_package in item["pip"]:
        print(pip_package)


if __name__ == "__main__":
    main()
