import sys
import torch
import ksc

import importlib

sys.path.append(ksc.utils.get_ksc_dir() + "/examples/dl-capsule")

mod = importlib.import_module("sqrl")


def test_vsqrl_vs_sqrl():
    x = torch.rand(10, 3, 4)
    ans = mod.vsqrl_pytorch(x)
    for i in 0, 1, 4, 9:
        ans_i = mod.sqrl_pytorch(x[i])
        assert torch.isclose(ans[i], ans_i)


def test_knossos_vs_pytorch():
    x = torch.rand(10, 3, 4)
    ans_pt = mod.vsqrl_pytorch(x)
    ans_ks = mod.vsqrl(x)
    assert torch.isclose(ans_pt, ans_ks).all()
