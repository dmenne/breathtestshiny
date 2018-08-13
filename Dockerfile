FROM dmenne/stanverse:latest

MAINTAINER Dieter Menne "dieter.menne@menne-biomed.de"

RUN Rscript -e "devtools::install_github(paste0('dmenne/', \
  c( 'breathtestcore', 'breathtestshiny', 'breathteststan')))" \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/*

COPY shiny-server.sh /usr/bin/shiny-server.sh

# Links to apps
RUN ln -s  /usr/local/lib/R/site-library/breathtestshiny/shiny /srv/shiny-server/breathtestshiny
# EXPOSE 3838 # already in stanverse

CMD ["/usr/bin/shiny-server.sh"]

