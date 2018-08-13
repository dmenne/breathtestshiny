FROM dmenne/stanverse:latest

MAINTAINER Dieter Menne "dieter.menne@menne-biomed.de"

# https://towardsdatascience.com/creating-sandbox-environments-for-r-with-docker-def54e3491a3
RUN Rscript -e "devtools::install_github(paste0('dmenne/', \
  c( 'breathtestcore', 'breathtestshiny', 'breathteststan')))" \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/*

# EXPOSE 3838 # already in stanverse

CMD ["Rscript", "-e", "breathtestshiny::run_shiny()"]

