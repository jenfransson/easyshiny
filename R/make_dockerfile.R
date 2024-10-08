#' @title Create Dockerfile
#' @description Create a dockerfile to build a docker image
#' @param path Path where to create Dockerfile
#' @return Does not return anything. Writes a Dockerfile to current working directory or path specified.
#' @author Roy Francis
#' @importFrom readr write_file
#' @export
#'
make_dockerfile <- function(path="."){
  make_shiny_config(path=path)
  x <- 
'# easyshiny

FROM rocker/shiny

RUN apt-get update && \\
    apt-get upgrade -y && \\
    apt-get clean && \\
    apt-get install -y git libxml2-dev libudunits2-dev libhdf5-dev && \\
    rm -rf /var/lib/apt/lists/*

RUN Rscript -e \'reqPkg = c("bslib","data.table", "DT", "ggdendro", "ggplot2", "ggplotify", "ggrepel", "glue", "grid", "hdf5r", "magrittr", "Matrix", "patchwork", "RColorBrewer", "readr", "remotes", "reticulate", "Seurat", "shiny", "shinycssloaders", "shinyhelper"); newPkg = reqPkg[!(reqPkg %in% installed.packages()[,"Package"])]; if(length(newPkg)){install.packages(newPkg)}\'

RUN mkdir /srv/shiny-server/app
COPY . /srv/shiny-server/app
COPY shiny-server.config /etc/shiny-server/shiny-server.conf
RUN sudo chown -R shiny:shiny /srv/shiny-server/app

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
'
  readr::write_file(x, file = file.path(path,"Dockerfile"))
}
