Dockerize: ds-shiny-tools
--------------------------------------------------------

author: brandon.wang@catalina.com  
Dockerize the repo: https://bitbucket.org/cameosaas/ds-shiny-tools/admin  
Update: 2020-05-20  

--------------------------------------------------------

Document the process to create the docker image and deploy ds-shiny-tools.
We have two deployment approach:
* VM
* Azure Container Instance 

--------------------------------------------------------

## Prepare Docker Images
Use docker as a tool to deploy shiny app is very continent. First, we create a docker image which could work as shiny server
for the shiny app. Then we deploy the images to a registry to hold a container. Now we can use the IP address to access the app. 

### Dockerfile

* We use `rocker/shiny-verse` as the base images, which is a shiny server image with tidyverse package installed
* The image will create an environment based on Debian
* Copy the `ds-shiny-tools` into the image folder `/srv/shiny-server/`
* Connection to Yellowbrick (DB)
    * Install `odbc-postgresql`
    * Load data sources credential (`odbc.ini`) to the image
    * Use IP address instead of hostname
* Connection to Cameo Dropbox
    * Install `openssh-client` package to use `scp` to upload file
    * Load private key and known_hosts to the image
    * Need to use `chown` and `chmod` to allow access
        * Shiny is running on user `shiny` not `root`
* Copy the `shiny-server.conf` to the image
    * Control the configuration for the shiny server
* Port to 80    

### Modify `ds-shiny-tools` for Docker Deployment

* Change RDS files location and upload separately
    * RDS files should not be packaged along with the docker images. Instead we are using volume mount.
    * Upload RDS to VM (_zaprshinyvm1:10.165.27.68_) `/shiny_data`
    * Change the RDS address to the pattern `/data/rds/...`
* Update codes in `/modules/cameoPage.R` 
    * Use IP address instead of hostname for Cameo dropbox

--------------------------------------------------------
## Build a local image to test

Build and run the docker images

`/data_docker` should be a local folder containing a sub-folder `/rds`, which has all input rds data files in total.

* mount `/data_docker` to `/data` to load RDS (Mac Version via Terminal)
```shell
docker build -t shiny .
docker run -d -p 80:80 -v /Users/bwang/data_docker:/data shiny
```
The site can be tested locally on `localhost/ds-shiny-tools/`.

* mount `/data_docker` to `/data` to load RDS (Windows Version via PowerShell)
```shell
docker build -t shiny .
docker run -d -p 80:80 -v C:\Users\qfan\Documents\data_docker:/data shiny
```

* Testing or debugging
```shell
docker ps # find the container id
docker exec -it container_id /bin/bash # interactive mode
```
refer to log information in `/var/log/shiny-server` for debugging

Stop the docker images
```shell
docker ps # find the container id
docker stop container_id
```

--------------------------------------------------------
## Deployment

### Deploy on VM (_zaprshinyvm1:10.165.27.68_)

```shell
# upload files from local to remote
scp -r ~/data_docker/data/rds/* shinyuser@10.165.27.68:/shiny_data/rds/

# build docker images
docker build -t shiny .
docker run -d -p 10.165.27.68:80:80 -v /shiny_data:/data shiny
```

Access to the shiny app [http://10.165.27.68/ds-shiny-tools/](http://10.165.27.68/ds-shiny-tools/)

### Deploy on Catalina Azure Container Registry (ACR)

* Access credentials to Catalina ACR are configured for shinyuser@10.165.27.68  
* Build the images and testing before pull the images since we don't have permission to delete images from ACR
* ACR does not allow to map ports.
* Tag and then pull to ACR
    * project: datasolutions
    * image: shiny

```shell
docker tag shiny:latest catalina.azurecr.io/datasolutions/shiny:v1
docker push catalina.azurecr.io/datasolutions/shiny:v1
```
* Access to the shiny app: [http://10.165.143.132/ds-shiny-tools/](http://10.165.143.132/ds-shiny-tools/)
* Manage container Instance: email brandon.wang@catalina.com for access. 
* File folders to load RDS data: email brandon.wang@catalina.com for access. 


--------------------------------------------------------
## Pipeline: updating shiny app 

1. Update code in [ds-shiny-tools](https://bitbucket.org/cameosaas/ds-shiny-tools/src/master/)
    * add `README.md` to notice user about the change
    * need to create a folder at `/data`
2. Load files to [docker-ds-portal](https://bitbucket.org/cameosaas/docker-ds-portal/src/master/)
3. Login *shinyuser@10.165.27.68* to build a new image to test
    * Folder: `/home/shinyuser/docker-ds-shiny-tools`
4. Tag and then push the image to ACR