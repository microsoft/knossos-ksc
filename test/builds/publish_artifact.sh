set -e

. test/builds/set-dirs.sh

mkdir -p Artifact
cp obj/test/ksc/gmm.cpp Artifact
cp src/runtime/knossos.h Artifact
cp src/runtime/knossos.cpp Artifact
cp ${PROF}-obj-functions.txt Artifact
cp ${PROF}-obj-lines.txt Artifact
cp ${PROF}-obj.out Artifact
cp ${PROF}-rev-functions.txt Artifact
cp ${PROF}-rev-lines.txt Artifact
cp ${PROF}-rev.out Artifact
git rev-parse HEAD > Artifact/git-rev-parse-HEAD.txt
