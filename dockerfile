#--- Image docker pour l'application MapYourGrid
FROM rocker/geospatial:latest

# Install dépendances système supplémentaires
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    curl \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /data
    
# Install quarto
RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    dpkg -i quarto-linux-amd64.deb && \
    rm quarto-linux-amd64.deb

# Vérif installation quarto
RUN which quarto && quarto --version

# Vérif installation sf 
RUN R -e "library(sf); cat('sf version:', as.character(packageVersion('sf')), '\n')"

# Install les autres packages R
RUN R -e "install.packages(c('shiny','quarto','plotly','scales','gt','gtExtras','janitor','leaflet','leafem','leaflet.extras2','bslib','bsicons','glue','jsonlite','rrapply','rvest','rio'))"

# Créa utilisateur applicatif
RUN groupadd --gid 10001 -r shiny \
    && useradd --uid 10001 -d /home/shiny -m -r -s /bin/bash -g shiny shiny

# Config shiny et chemin quarto
RUN mkdir -p $(R RHOME)/etc && \
    echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > $(R RHOME)/etc/Rprofile.site && \
    echo "Sys.setenv(PATH = paste('/usr/local/bin', Sys.getenv('PATH'), sep=':'))" >> $(R RHOME)/etc/Rprofile.site

# Créa utilisateur applicatif
RUN groupadd --gid 10001 -r shiny \
    && useradd --uid 10001 -d /home/shiny -m -r -s /bin/false -g shiny shiny
    
WORKDIR /home/app
COPY --chown=shiny:shiny . .

# Rendre le script exécutable
RUN chmod +x docker-entrypoint.sh \
    && chown -R shiny:shiny /data \
    && chmod -R 755 /data \
    && chown -R shiny:shiny . \
    && chmod -R 755 .

USER shiny
EXPOSE 3838
ENTRYPOINT ["./docker-entrypoint.sh"]
