# Shiny Apps Followup

## Deploy Steps for Shiny on Docker

* Push the latest code updates to the remote Repo `docker-ds-portal`
* Upload the data file to the VM: `/shiny_data/rds/`
* Go to the VM `/home/shinyuser/docker-ds-portal`
* Pull the latest commit
* Build the docker images: `$ docker build -t shiny .`
    * Use the Dockerfile to build the docker image and set up the environment
    * All confidential files will be uploaded to the VM manually
* Run the new docker image: `$ docker run --rm -d -p 10.165.27.68:80:80 -v /shiny_data:/data shiny`
* Disable and remove the old docker containers/images

## Deploy Steps for Shiny on VM

* Push the latest code updates to the remote Repo `ds-shiny-tools`
* Go to the directory in the VM: `/srv/shiny-server/test-apps/ds-shiny-tools`
* Upload the test data file to the VM: `/srv/shiny-server/test-apps/ds-shiny-tools/rds/`
* Pull the latest commit
* Restart the shiny-server to reload the app
* Note:
    * Since this is a demo repo, some functions are disabled due to safety issues.
    * This directory is currently unable to access. 