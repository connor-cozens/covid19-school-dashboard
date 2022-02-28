FROM rocker/shiny:4.0.5

RUN apt-get update && apt-get install -y \
    r-base gdebi-core \
    r-base \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev

RUN R -e 'install.packages(c(\
              "shiny", \
              "shinydashboard", \
              "ggplot2", \
              "DT", \
              "reshape2", \
              "rgdal", \
              "shinythemes", \
              "sp", \
              "plotly", \
              "xts", \
              "data.table", \
              "ggmap", \
              "httr", \
              "leaflet", \
              "lubridate", \
              "readxl", \
              "RColorBrewer", \
              "rgeos", \
              "rvest", \
              "stringr", \
              "stringdist", \
              "git2r", \
              "Rcpp" \
            ), \
            repos="https://packagemanager.rstudio.com/all/__linux__/focal/latest"\
          )'

COPY ./shiny-app/data /srv/shiny-server/data
COPY ./shiny-app/data/shapefiles /srv/shiny-server/data/shapefiles
COPY ./shiny-app/* /srv/shiny-server/
COPY ./shiny-app/www /srv/shiny-server/www

CMD ["/usr/bin/shiny-server"]