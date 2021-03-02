set -e

. test/builds/set-dirs.sh

mkdir -p $PROFDIR
./build/bin/ksc --profile --ks-file-without-extension test/ksc/run-gmm-obj-for-profiling \
    --proffile ${PROF}-obj.out --proffunctions ${PROF}-obj-functions.txt --proflines ${PROF}-obj-lines.txt
./build/bin/ksc --profile --ks-file-without-extension test/ksc/run-gmm-rev-for-profiling \
    --proffile ${PROF}-rev.out  --proffunctions ${PROF}-rev-functions.txt --proflines ${PROF}-rev-lines.txt
