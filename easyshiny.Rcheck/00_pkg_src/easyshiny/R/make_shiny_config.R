#' @title Create config for shiny server
#' @description Create config for shiny server
#' @param path Path where to create file
#' @return Does not return anything. Writes a "shiny-server.config" to current working directory or path specified.
#' @author Roy Francis
#' @importFrom readr write_file
#' @export
#'
make_shiny_config <- function(path="."){
  x <- 
'run_as shiny;
http_keepalive_timeout 1200;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Define a location at the base URL
  location / {

    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server/app;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
    app_init_timeout 300;
    app_idle_timeout 600;
  }
  app_init_timeout 300;
  app_idle_timeout 600;
}
'
  readr::write_file(x, file = file.path(path,"shiny-server.config"))
}
