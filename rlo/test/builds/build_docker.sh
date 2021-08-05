set -e

dockerPassword=$1
dockerId=knossos
repository=${dockerId}.azurecr.io
#Version on the Dockerfile and the conda-env.yaml that we copy in.
VERSION=$(./test/builds/docker_tag.sh)
imageName=rlo_linux_base:${VERSION}
fullName=${repository}/${imageName}

cp ./test/builds/conda-env.yaml ./test/builds/Docker/
cd ./test/builds/Docker
docker login -u ${dockerId} -p ${dockerPassword} ${repository}

echo Looking for Docker Image $fullName
if (docker pull $fullName); then
  echo found $fullName
else
  echo did not find $fullName
  # To see output from RUN commands in stdout when using newer versions of docker 
  # (since early 2021), invoke `docker build` with `--progress=plain`
  docker build -t $fullName .
  docker push $fullName
fi
