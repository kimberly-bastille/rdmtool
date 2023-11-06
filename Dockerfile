FROM rocker/shiny:4.3
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY . /srv/rdmtool/
RUN Rscript /srv/rdmtool/R/required_packages.R