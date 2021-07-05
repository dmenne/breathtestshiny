FROM rocker/shiny-verse:latest

LABEL maintainer="dieter.menne@menne-biomed.de"

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libv8-dev

RUN install2.r --error --ncpus 2 --deps TRUE --skipinstalled \
    DT \
    gtools \
    shinyjs \
    shinythemes \
    shinyAce \
    shinycssloaders

RUN mkdir -p ~/.R
RUN echo "CXX14FLAGS=-O3 -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined -Wno-deprecated-declarations -Wno-ignored-attributes" >> ~/.R/Makevars


RUN install2.r --error --ncpus 2 --deps TRUE --skipinstalled \
   rstan \
   bayesplot \
   rstantools


RUN Rscript -e "devtools::install_github(paste0('dmenne/', \
  c( 'breathtestcore', 'breathtestshiny')))"

RUN Rscript -e "devtools::install_github('dmenne/breathteststan')" \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/*

COPY shiny-server.sh /usr/bin/shiny-server.sh
# https://github.com/rocker-org/shiny/issues/32
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
COPY shiny-server.conf /etc/shiny-server

# Remove apps
RUN rm -R /srv/shiny-server
# Links to breathtestshiny
RUN ln -s  /usr/local/lib/R/site-library/breathtestshiny/shiny /srv/shiny-server
# EXPOSE 3838 # already in stanverse
HEALTHCHECK --interval=60s CMD curl --fail http://localhost:3838 || exit 1

CMD ["/usr/bin/shiny-server.sh"]

