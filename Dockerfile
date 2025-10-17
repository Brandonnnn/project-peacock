# get shiny servers image with tidyverse package installed / Debian
FROM rocker/shiny

# install system libraries of general use
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev libxt-dev libssl-dev \
    vim sudo curl openssh-client \
    unixodbc unixodbc-dev odbc-postgresql

# setup DB driver
COPY ./secrets/odbc.ini /etc/odbc.ini

# copy credentials needed for dropbox
COPY ./secrets/id_rsa /home/shiny/.ssh/id_rsa
COPY ./secrets/known_hosts /home/shiny/.ssh/known_hosts
RUN sudo chown -R shiny:shiny /home/shiny/.ssh/id_rsa
RUN sudo chmod 600 /home/shiny/.ssh/id_rsa

# install azure cli
RUN curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash
COPY ./secrets/upload2Az.sh /home/upload2Az.sh
RUN sudo chmod u+x /home/upload2Az.sh
RUN touch /home/log.txt
RUN sudo chmod 766 /home/log.txt

# install Java 8 and ybtools
RUN sudo apt update
RUN sudo apt install -y openjdk-8-jdk
COPY ./packages/ybtools-4.1.4.tar /tmp/ybtools-4.1.4.tar
RUN tar -xf /tmp/ybtools-4.1.4.tar -C /home/
COPY ./secrets/.ybpassword /home/shiny/.ybpassword

# Install required R packages
WORKDIR /tmp
COPY ./requirements.R /tmp/requirements.R
RUN Rscript /tmp/requirements.R

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# copy the app to the image
COPY ds-shiny-tools /srv/shiny-server/ds-shiny-tools
COPY ds-portal-data /srv/shiny-server/ds-portal-data
COPY ds-utility-page /srv/shiny-server/ds-utility-page

# select port
EXPOSE 80

# run app
CMD ["/usr/bin/shiny-server"]
