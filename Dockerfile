# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y --no-install-recommends apt-utils
RUN DEBIAN_FRONTEND=noninteractive apt-get update -qq && apt-get -y install \
libcurl4-openssl-dev \
libgdal-dev \
openjdk-11-jdk \
libbz2-dev

## update system libraries
RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
apt-get upgrade -y && \
apt-get clean

# copy necessary files
## app folder
COPY / ./
## renv.lock file
COPY /renv.lock ./renv.lock

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/', host = '0.0.0.0', port = 3838)"]