# Overview
"Amulet"/"amlt", formerly known as "pt", formerly known as "philly tools", is a tool for submitting experiments to remote compute resources. See the [Amulet documentation](https://phillytools.azurewebsites.net/). 


# Install Amulet
You should install Amulet in a separate conda env or virtual environment, not your regular Knossos dev environment, since it requires packages that may conflict with those in the Knossos environment.
You can follow instructions [here](https://phillytools.azurewebsites.net/master/setup.html) or use `./create_amlt_env.sh` to create a conda environment called `amlt` with `amlt` installed.

# You no longer need access keys!
With old versions of `pt` it was necessary to get access keys for our Docker container registry and for the knossosphillystorage blob store.  You no longer need to do this.

# Write the philly config YAML file
See examples in this directory, and documentation for the config file format [here](https://phillytools.azurewebsites.net/master/config_file.html).

To run in the Knossos AzureML workspace, the target and environment sections of the philly config file should be as follows (substitute whatever cluster you want):
```
target:
  service: aml
  cluster: v100cluster  
  subscription_id: "0f64a630-b912-4501-ad57-262a9e12637c"
  workspace_name: knossosws
  resource_group: knossosamlrg

environment:
  image: knossos.azurecr.io/rlo_linux_base:$DOCKER_TAG
  registry: knossos.azurecr.io
  username: 00000000-0000-0000-0000-000000000000
```
The environment variable `$DOCKER_TAG` will be set automatically if you run the job using `amlt_runner.sh`. If you find that the tag you get by running `docker_tag.sh` locally does not match the tag in the UbuntuDockerBuild part of Build And Test running in CI, try `git config --global core.autocrlf True`.


To run in GCR resrchvc workspace, on a Kubernetes cluster, see the 'AMLK8s' section in the Amulet config file docs [here](https://phillytools.azurewebsites.net/master/config_file.html) and examples such as `train_blas_itp.yaml` in this directory.

# Check out an amlt project
Before running a job using Amulet you must check out a project:
```
$ amlt project checkout PROJECT_NAME knossosphillystorage
```
# Submit the job
Use `scripts/amlt_runner.sh` to submit the job.  `amlt_runner` sets some environment variables and then invokes `amlt run`. [Documentation](https://phillytools.azurewebsites.net/master/reference/60_cli_reference.html) for amlt run.  
If you want to upload data for a run, then addition to populating the `data` section of the philly config YAML file, you must invoke `amlt run` with the `--upload-data` flag.

Example: from root of Knossos repo, do
```
$ scripts/amlt_runner.sh amulet_configs/train_on_dataset.yaml --upload-data
```

# Troubleshooting: delete stale passwords
Out-of-date passwords stored by Amulet can cause authentication errors.  We have seen this happen when AzureML attempted to fetch Docker images from the Knossos container registry. If you suspect this is happening, look for the stored password in `~/.config/amulet/vault.yml` and delete it.
