set -e

mkdir Artifact
cp obj/test/ksc/gmm.cpp Artifact
cp src/runtime/knossos.h Artifact
cp src/runtime/knossos.cpp Artifact
cp prof-obj-functions.txt Artifact
cp prof-obj-lines.txt Artifact
cp prof-obj.out Artifact
cp prof-rev-functions.txt Artifact
cp prof-rev-lines.txt Artifact
cp prof-rev.out Artifact
git rev-parse HEAD > Artifact/git-rev-parse-HEAD.txt
cp ksc Artifact
