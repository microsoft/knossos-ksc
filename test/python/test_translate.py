from ksc.translate import translate


def test_translate_resnet():
    input_ks_file = "examples/dl-resnet/resnet_v2.ks"
    backend = "jax_input_last"

    with open(input_ks_file) as f:
        print(translate(f.read(), backend, input_ks_file))

    assert True
