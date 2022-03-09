# syntax=docker/dockerfile:experimental
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y --no-install-recommends apt-utils
RUN DEBIAN_FRONTEND=noninteractive apt-get update -qq && apt-get -y install \
libcurl4-openssl-dev \
libgdal-dev \
openjdk-11-jdk \
libbz2-dev \
ssh \
git
    
## update system libraries
RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
apt-get upgrade -y && \
apt-get clean

# Download public key for github.com
# RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts

# RUN --mount=type=ssh ssh-add -L

# Clone private repository
# RUN --mount=type=ssh git clone git@gitlab.com:br00t/ontario-covid19-dashboard.git
# RUN git config --global user.name "Peter Taylor"
# RUN git config --global user.email "peter.taylor@taypeinternational.com"

# copy necessary files
## app folder
COPY / ./
## renv.lock file
COPY /renv.lock ./renv.lock

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(TRUE); renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
# CMD ["R", "-e", "shiny::runApp('/ontario-covid19-dashboard', host = '0.0.0.0', port = 3838)"]
CMD ["R", "-e", "shiny::runApp('/', host = '0.0.0.0', port = 3838)"]