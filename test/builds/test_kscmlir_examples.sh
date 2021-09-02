set -e

KNOSSOS=$1
KSCPY=$KNOSSOS/src/python

mkdir -p obj/test/kscmlir/examples

for example in ex0; do
    echo "Testing example $example.ks"
    PYTHONPATH=$KSCPY python3 src/python/ksc/prune_and_compile.py test/ksc/$example.ks obj/test/kscmlir/examples/$example.kso
    ./build/bin/ksc-mlir MLIR obj/test/kscmlir/examples/$example.kso > /dev/null
done
