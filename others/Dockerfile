FROM rocker/shiny

# # get shiny servers image with tidyverse package installed / Debian
# FROM openjdk:8-jre

# # install system libraries of general use
# RUN apt-get update && apt-get install -y libcurl4-gnutls-dev libxt-dev libssl-dev vim sudo curl openssh-client

# # Install R and Shiny
# WORKDIR /tmp
# RUN sudo apt-get install -y r-base
# RUN Rscript -e "install.packages('shiny', repos='https://cran.rstudio.com/')"
# RUN sudo apt-get install -y gdebi-core

# # Install required R packages
# WORKDIR /tmp
# COPY ./requirements.R /tmp/requirements.R
# RUN Rscript /tmp/requirements.R

# # install ODBC driver for PostgreSQL
# RUN apt-get install odbc-postgresql
# COPY ./secrets/odbc.ini /etc/odbc.ini

# # copy credentials needed for dropbox
# COPY ./secrets/id_rsa /home/shiny/.ssh/id_rsa
# COPY ./secrets/known_hosts /home/shiny/.ssh/known_hosts
# RUN sudo chown -R shiny:shiny /home/shiny/.ssh/id_rsa
# RUN sudo chmod 600 /home/shiny/.ssh/id_rsa

# # install azure cli
# RUN curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash
# COPY ./secrets/upload2Az.sh /home/upload2Az.sh
# RUN sudo chmod u+x /home/upload2Az.sh
# RUN touch /home/log.txt
# RUN sudo chmod 766 /home/log.txt

# # Copy configuration files into the Docker image
# COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# # copy the app to the image
# COPY ds-shiny-tools /srv/shiny-server/ds-shiny-tools

# select port
# EXPOSE 80

# # run app
# CMD ["/usr/bin/shiny-server.sh"]
