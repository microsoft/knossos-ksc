### ONNX2KS: Convert onnx files to Knossos IR

## Building

Install onnxruntime
```sh
$ cd ../onnxruntime
$ ./build.sh --enable_training --gen_doc  --config=RelWithDebInfo --build_wheel
$ pip install ./build/Linux/RelWithDebInfo/dist/onnxruntime-1.5.2-cp38-cp38-linux_x86_64.whl
```

And ensure you're using the compatible ONNX
```sh
$ cd ../onnxruntime/cmake/external/onnx
$ python setup.py install
```
