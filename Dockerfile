FROM rocker/geospatial:latest

RUN apt-get update && apt-get install -y 


RUN Rscript -e 'remotes::install_version("bslib", upgrade = "never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("config", upgrade = "never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("DT", upgrade = "never", version = "0.31")'
RUN Rscript -e 'remotes::install_version("ggh4x", upgrade = "never", version = "0.2.8")'
RUN Rscript -e 'remotes::install_version("mapedit", upgrade = "never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("patchwork", upgrade = "never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("shinyjs", upgrade = "never", version = "2.1.0")'

COPY . /srv/shiny-server/gotedna
WORKDIR /srv/shiny-server/gotedna
RUN Rscript -e 'remotes::install_local(upgrade = "never", dependencies = TRUE)'

# prep data directory; when deploy in k8s this folder will be backended to a NAS containing all fst files
RUN mkdir /data
RUN chmod 0777 /data
RUN ln -s /data /srv/shiny-server/gotedna/inst/app/data

# expose container port 9292
EXPOSE 9292

# serve the application on startup
WORKDIR /srv/shiny-server/gotedna/inst/app
CMD ["Rscript", "-e", "options('shiny.port' = 9292, shiny.host = '0.0.0.0');  shiny::runApp()"]
