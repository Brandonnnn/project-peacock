# get shiny servers image with tidyverse package installed / Debian
FROM rocker/shiny

# install system libraries of general use
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev libxt-dev libssl-dev \
    vim sudo curl openssh-client \

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# Install required R packages
WORKDIR /tmp
COPY ./requirements.R /tmp/requirements.R
RUN Rscript /tmp/requirements.R

# copy the app to the image
COPY peacock-insight /srv/shiny-server/peacock-insight

# select port
EXPOSE 80

# run app
CMD ["/usr/bin/shiny-server"]
