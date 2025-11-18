#--- Image docker pour l'application MapYourGrid
FROM rocker/tidyverse:latest

# Install dépendances système 
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    curl \
    git \
    openssh-client \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*
    
# Install quarto
RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    dpkg -i quarto-linux-amd64.deb && \
    rm quarto-linux-amd64.deb

# Vérif installation quarto
RUN which quarto && quarto --version

# Install packages R
RUN R -e "install.packages(c('shiny','quarto','plotly','scales','gt','gtExtras','janitor','sf','leaflet','leafem','leaflet.extras2','bslib','bsicons'))"

# Config shiny et chemin quarto
RUN mkdir -p $(R RHOME)/etc && \
    echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > $(R RHOME)/etc/Rprofile.site && \
    echo "Sys.setenv(PATH = paste('/usr/local/bin', Sys.getenv('PATH'), sep=':'))" >> $(R RHOME)/etc/Rprofile.site

WORKDIR /home/app
COPY . .

EXPOSE 3838

CMD ["R", "-e", "quarto::quarto_serve('application_OET.qmd')"]
