FROM openanalytics/r-base

LABEL maintainer "Albert Gehami <gehami@alumni.stanford.edu>"



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
    libssl1.0.0

# system library dependency for the govGeneralMap app
RUN apt-get update && apt-get install -y \
    libgeos++   


# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'rsconnect'), repos='https://cloud.r-project.org/')"

# install dependencies of the general map app
RUN R -e "install.packages(c('shinyWidgets', 'tools', 'hash', 'leaflet', 'leaflet.minicharts', 'magrittr', 'rgdal', 'sp', 'rgeos', 'shinyBS','shinyjs', 'htmltools'), repos='https://cloud.r-project.org/')"



# copy the app to the image
RUN mkdir /root/govGeneralMap
COPY . /root/govGeneralMap

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/govGeneralMap')"]
