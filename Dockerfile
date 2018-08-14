FROM dmenne/stanverse:latest

MAINTAINER Dieter Menne "dieter.menne@menne-biomed.de"

RUN install2.r --error  devtools

RUN Rscript -e "devtools::install_github(paste0('dmenne/', \
  c( 'breathtestcore', 'breathtestshiny')))"

RUN Rscript -e "devtools::install_github('dmenne/breathteststan')"

#  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
#  && rm -rf /var/lib/apt/lists/*

COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY shiny-server.conf /etc/shiny-server

# Links to apps
RUN ln -s  /usr/local/lib/R/site-library/breathtestshiny/shiny /srv/shiny-server/breathtestshiny
# EXPOSE 3838 # already in stanverse

CMD ["/usr/bin/shiny-server.sh"]

