FROM dmenne/stanverse:latest

MAINTAINER Dieter Menne "dieter.menne@menne-biomed.de"

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

CMD ["/usr/bin/shiny-server.sh"]

