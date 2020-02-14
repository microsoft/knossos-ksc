import argparse
import hashlib
import json
import numpy as np
import os
from PIL import Image
from urllib.request import urlretrieve

INPUT_FILE_URL = "https://knossosbuildpipeline.blob.core.windows.net/static/imagenet/msrc.jpg"
PYTORCH_RESNET50_ORIGINAL_URL = "https://download.pytorch.org/models/resnet50-19c8e357.pth"
PYTORCH_RESNET50_URL = "https://knossosbuildpipeline.blob.core.windows.net/static/imagenet/resnet50.npz"
CLASS_INDEX_URL = "https://knossosbuildpipeline.blob.core.windows.net/static/imagenet/imagenet_class_index.json"

# taken from https://github.com/keras-team/keras/blob/master/keras/utils/data_utils.py
def _hash_file(path, chunk_size=65535):
    hasher = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(chunk_size), b''):
            hasher.update(chunk)
    return hasher.hexdigest()

def download_file(file_url, md5_hash):
    local_filename = file_url.split("/")[-1]
    if not os.path.isfile(local_filename):
        print(f"Downloading {file_url} to {local_filename}")
        urlretrieve(file_url, local_filename)
    assert _hash_file(local_filename) == md5_hash
    return local_filename

def resnet50_v2_weights_from_pytorch(p):
    num_blocks = [3, 4, 6, 3]

    return (
        (
            np.array([0.485, 0.456, 0.406], dtype=np.float32), # mean
            np.array([0.229, 0.224, 0.225], dtype=np.float32)  # std
        ),
        p["conv1.weight"],
        (
            p["bn1.running_mean"],
            p["bn1.running_var"],
            p["bn1.weight"],
            p["bn1.bias"]
        ),
        [
            (
                (
                    (
                        p[f"layer{i+1}.0.conv1.weight"],
                        (
                            p[f"layer{i+1}.0.bn1.running_mean"],
                            p[f"layer{i+1}.0.bn1.running_var"],
                            p[f"layer{i+1}.0.bn1.weight"],
                            p[f"layer{i+1}.0.bn1.bias"]
                        ),
                        p[f"layer{i+1}.0.conv2.weight"],
                        (
                            p[f"layer{i+1}.0.bn2.running_mean"],
                            p[f"layer{i+1}.0.bn2.running_var"],
                            p[f"layer{i+1}.0.bn2.weight"],
                            p[f"layer{i+1}.0.bn2.bias"]
                        ),
                        p[f"layer{i+1}.0.conv3.weight"],
                        (
                            p[f"layer{i+1}.0.bn3.running_mean"],
                            p[f"layer{i+1}.0.bn3.running_var"],
                            p[f"layer{i+1}.0.bn3.weight"],
                            p[f"layer{i+1}.0.bn3.bias"]
                        ),
                    ),
                    p[f"layer{i+1}.0.downsample.0.weight"],
                    (
                        p[f"layer{i+1}.0.downsample.1.running_mean"],
                        p[f"layer{i+1}.0.downsample.1.running_var"],
                        p[f"layer{i+1}.0.downsample.1.weight"],
                        p[f"layer{i+1}.0.downsample.1.bias"]
                    )
                ),
                [
                    (
                        p[f"layer{i+1}.{j}.conv1.weight"],
                        (
                            p[f"layer{i+1}.{j}.bn1.running_mean"],
                            p[f"layer{i+1}.{j}.bn1.running_var"],
                            p[f"layer{i+1}.{j}.bn1.weight"],
                            p[f"layer{i+1}.{j}.bn1.bias"]
                        ),
                        p[f"layer{i+1}.{j}.conv2.weight"],
                        (
                            p[f"layer{i+1}.{j}.bn2.running_mean"],
                            p[f"layer{i+1}.{j}.bn2.running_var"],
                            p[f"layer{i+1}.{j}.bn2.weight"],
                            p[f"layer{i+1}.{j}.bn2.bias"]
                        ),
                        p[f"layer{i+1}.{j}.conv3.weight"],
                        (
                            p[f"layer{i+1}.{j}.bn3.running_mean"],
                            p[f"layer{i+1}.{j}.bn3.running_var"],
                            p[f"layer{i+1}.{j}.bn3.weight"],
                            p[f"layer{i+1}.{j}.bn3.bias"]
                        )
                    ) for j in range(1, num_blocks[i])
                ]
            ) for i in range(len(num_blocks))
        ],
        (
            p["fc.weight"],
            p["fc.bias"]
        )
    )

def resnet50_py_weights_from_pytorch(p):
    num_blocks = [3, 4, 6, 3]

    return (
        (
            np.array([0.485, 0.456, 0.406], dtype=np.float32), # mean
            np.array([0.229, 0.224, 0.225], dtype=np.float32)  # std
        ),
        p["conv1.weight"],
        (
            p["bn1.running_mean"],
            p["bn1.running_var"],
            p["bn1.weight"],
            p["bn1.bias"]
        ),
        tuple(
            (
                (
                    (
                        p[f"layer{i+1}.0.conv1.weight"],
                        (
                            p[f"layer{i+1}.0.bn1.running_mean"],
                            p[f"layer{i+1}.0.bn1.running_var"],
                            p[f"layer{i+1}.0.bn1.weight"],
                            p[f"layer{i+1}.0.bn1.bias"]
                        ),
                        p[f"layer{i+1}.0.conv2.weight"],
                        (
                            p[f"layer{i+1}.0.bn2.running_mean"],
                            p[f"layer{i+1}.0.bn2.running_var"],
                            p[f"layer{i+1}.0.bn2.weight"],
                            p[f"layer{i+1}.0.bn2.bias"]
                        ),
                        p[f"layer{i+1}.0.conv3.weight"],
                        (
                            p[f"layer{i+1}.0.bn3.running_mean"],
                            p[f"layer{i+1}.0.bn3.running_var"],
                            p[f"layer{i+1}.0.bn3.weight"],
                            p[f"layer{i+1}.0.bn3.bias"]
                        ),
                    ),
                    p[f"layer{i+1}.0.downsample.0.weight"],
                    (
                        p[f"layer{i+1}.0.downsample.1.running_mean"],
                        p[f"layer{i+1}.0.downsample.1.running_var"],
                        p[f"layer{i+1}.0.downsample.1.weight"],
                        p[f"layer{i+1}.0.downsample.1.bias"]
                    )
                ),
            )
            + tuple(
                (
                    p[f"layer{i+1}.{j}.conv1.weight"],
                    (
                        p[f"layer{i+1}.{j}.bn1.running_mean"],
                        p[f"layer{i+1}.{j}.bn1.running_var"],
                        p[f"layer{i+1}.{j}.bn1.weight"],
                        p[f"layer{i+1}.{j}.bn1.bias"]
                    ),
                    p[f"layer{i+1}.{j}.conv2.weight"],
                    (
                        p[f"layer{i+1}.{j}.bn2.running_mean"],
                        p[f"layer{i+1}.{j}.bn2.running_var"],
                        p[f"layer{i+1}.{j}.bn2.weight"],
                        p[f"layer{i+1}.{j}.bn2.bias"]
                    ),
                    p[f"layer{i+1}.{j}.conv3.weight"],
                    (
                        p[f"layer{i+1}.{j}.bn3.running_mean"],
                        p[f"layer{i+1}.{j}.bn3.running_var"],
                        p[f"layer{i+1}.{j}.bn3.weight"],
                        p[f"layer{i+1}.{j}.bn3.bias"]
                    )
                ) for j in range(1, num_blocks[i])
            ) for i in range(len(num_blocks))
        ),
        (
            p["fc.weight"],
            p["fc.bias"]
        )
    )


def get_shape(weights):
    if isinstance(weights, tuple):
        return tuple(get_shape(el) for el in weights)
    if isinstance(weights, list):
        return list(get_shape(el) for el in weights)
    elif isinstance(weights, np.ndarray):
        return weights.shape
    else:
        raise ValueError(f"Found {type(weights)}!")

def get_class_name_dict(class_names_file):
    with open(class_names_file) as f:
        return json.load(f)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, choices=["resnet_v2", "resnet_py"])
    args = parser.parse_args()
    npz_file = download_file(
        PYTORCH_RESNET50_URL,
        "8947031f2fc1799404c49924d72a97c4"
    )
    imagenet_class_names_file = download_file(
        CLASS_INDEX_URL,
        "c2c37ea517e94d9795004a39431a14cb"
    )
    weights = np.load(npz_file)
    assert weights["_original_url"] == PYTORCH_RESNET50_ORIGINAL_URL

    input_file = download_file(
        INPUT_FILE_URL,
        "25d496494b2ae14f61fced9a4325ab08"
    )
    print(f"Loading image {input_file}")
    input = np.asarray(Image.open(input_file)).transpose((2, 0, 1))[None, :, :, :] / 255.0 # NCHW

    # disable name mangling so that the output looks more readable
    from ksc.tracing import jitting
    jitting.disable_name_mangling()
    if args.model == "resnet_v2":
        from resnet_v2 import Resnet50 as resnet
        weights = resnet50_v2_weights_from_pytorch(weights)
    elif args.model == "resnet_py":
        import sys
        sys.path.append("examples/dl-resnet")
        from resnet import resnet
        weights = resnet50_py_weights_from_pytorch(weights)
    
    if args.model == "resnet_py":
        out = resnet(input, weights)
        out = out.data
    else:
        out = resnet(weights, input)
    class_names = get_class_name_dict(imagenet_class_names_file)
    top5 = out.ravel().argsort()[-5:][::-1]
    print("\n".join([f"score={out[0, i]}, {class_names[str(i)]}" for i in top5]))
    assert class_names[str(top5[0])] == ["n03661043", "library"]
    assert class_names[str(top5[1])] == ["n02871525", "bookshop"]

if __name__ == "__main__":
    main()