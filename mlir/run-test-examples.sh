grep 'RUN.*%K' mlir/test/Ksc/examples.ks | sed -e 's|^; RUN: |./build/bin/|' | sed -e 's/%KNOSSOS/./' | sh -x
