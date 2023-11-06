FROM rocker/shiny:4.3
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY . /srv/rdmtool/
COPY ./recDST/app.R /srv/rdmtool/.
RUN install2.r -e -s \
    shiny \
    shinyjs \
    shinyWidgets \
    magrittr \
    readr \
    here \
    dplyr \
    tidyr \
    stringr \
    lubridate \
    tibble \
    data.table \
    knitr \
    openxlsx \
    plyr \
    markdown \
    && rm -rf /tmp/downloaded_packages \
    && chown -R shiny:shiny /srv/rdmtool