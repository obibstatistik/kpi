FROM openanalytics/r-base

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libpq-dev \
    libxml2-dev

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
# TODO: draw from your own basic docker image instead of r-base above, where
#       all or most of the Whitebook packages are already installed so we only
#	need to update them here and so build the containers quicker when we commit.
RUN R -e "install.packages(c('devtools','markdown','DiagrammeR','RPostgreSQL','shiny','shinydashboard','plotly','formattable','httr','curl','dplyr','foreach','openxlsx','shinyLP','data.table','data.tree','gridSVG','XML','lubridate','tidyr','ggplot2','profvis','leaflet','janitor','treemap','RColorBrewer'), repos='https://cloud.r-project.org/')"

# install dependencies from github
RUN R -e "devtools::install_github('sorenb/whitebookviz')"

# copy the app to the image
RUN mkdir /root/whitebook
COPY . /root/whitebook

# this file if included forces R to run the app on port 3838 instead of on a random port
# you could also do this further down: CMD ["R", "-e", "shiny::runApp('/root/whitebook', port = 3838)"]
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/whitebook')"]
