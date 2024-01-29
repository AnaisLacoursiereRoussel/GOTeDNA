# generated with golem::add_dockerfile()
FROM rocker/geospatial:latest

RUN apt-get update && apt-get install -y 

RUN Rscript -e 'remotes::install_version("bslib", upgrade = "never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("config", upgrade = "never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("DT", upgrade = "never", version = "0.31")'
RUN Rscript -e 'remotes::install_version("ggh4x", upgrade = "never", version = "0.2.8")'
RUN Rscript -e 'remotes::install_version("mapedit", upgrade = "never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("patchwork", upgrade = "never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("shinyjs", upgrade = "never", version = "2.1.0")'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

RUN Rscript -e 'remotes::install_local(upgrade = "never", depedencies = TRUE)'
WORKDIR /usr
RUN rm -rf /build_zone

EXPOSE 9292
CMD Rscript -e "options('shiny.port' = 9292, shiny.host = '0.0.0.0');  GOTeDNA::run_gotedna_app()"
