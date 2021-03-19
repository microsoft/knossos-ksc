set -e

mkdir -p obj/test/kscmlir/examples

for example in ex0; do
    echo "Testing example $example.ks"
    ./build/bin/ksc --generate-cpp \
        --ks-source-file src/runtime/prelude.ks \
        --ks-source-file test/ksc/$example.ks \
        --ks-output-file obj/test/kscmlir/examples/$example.kso \
        --cpp-output-file /dev/null
    ./build/bin/ksc-mlir MLIR obj/test/kscmlir/examples/$example.kso > /dev/null
done
