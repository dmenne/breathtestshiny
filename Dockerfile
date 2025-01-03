FROM rocker/r-base:latest


LABEL maintainer="dieter.menne@menne-biomed.de"

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libv8-dev \
  libxml2-dev \
  libcurl4-openssl-dev \
  libfontconfig1-dev \
  libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
  libharfbuzz-dev libfribidi-dev \
  curl


RUN install2.r --error --ncpus 6 --skipinstalled \
    DT \
    gtools \
    shiny \
    shinyjs \
    shinythemes \
    tidyverse \
    shinyAce \
    shinyBS \
    shinycssloaders \
    curl \
    xml2 \
  	V8 \
    httr \
    markdown \
    remotes

RUN mkdir -p ~/.R
RUN echo "CXX14FLAGS=-O3 -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined -Wno-deprecated-declarations -Wno-ignored-attributes" >> ~/.R/Makevars

RUN install2.r --error --ncpus 6 --skipinstalled \
   rstan \
   bayesplot \
   rstantools

RUN Rscript -e "remotes::install_github(paste0('dmenne/', \
  c( 'breathtestcore', 'breathtestshiny')))"

RUN Rscript -e "remotes::install_github('dmenne/breathteststan')" \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/*

EXPOSE 3838
HEALTHCHECK --interval=60s --start-period=20s CMD curl -I --fail http://localhost:3838 || exit 1

CMD ["R", "-e", \
 "shiny::runApp(system.file('shiny', package = 'breathtestshiny'), \
   host = '0.0.0.0', port = 3838)"]

