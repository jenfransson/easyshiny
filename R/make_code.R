#' @title Generate code files required for shiny app (one dataset)
#' @description Generate code files required for shiny app containing only one dataset. In particular, two R scripts will be generated, namely \code{server.R} and \code{ui.R}. If users want to include multiple dataset in one shiny app, please use \code{make_code_multi()} instead. Note that both \code{make_file} and \code{make_code} functions are ran when running the wrapper function \code{make_app}.
#' @param shiny.title title for shiny app
#' @param shiny.prefix specify file prefix
#' @param shiny.dir specify directory to create the shiny app in
#' @param enableSubset specify whether to enable "Toggle to subset cells" functionality in the shiny app. Default is to enable this functionality
#' @param defPtSiz specify default point size for single cells. For example, a smaller size can be used if you have many cells in your dataset
#' @param theme Bootsrap theme
#' @param tabs Vector of tab names to include
#' @param font Google font for plots. Defaults to "Lato". Requires package `showtext`.
#' @param ganalytics Google analytics tracking ID (e.g. "UA-123456789-0")
#' @return server.R and ui.R required for shiny app
#' @author John F. Ouyang
#' @author Roy Francis
#' @importFrom readr write_file
#' @import data.table readr glue shiny
#' @importFrom utils packageVersion
#' @importFrom ggplotify as.ggplot
#' @export
#' 
make_code <- function(shiny.title = "App", shiny.prefix = "sc1", shiny.dir = "app", enableSubset = TRUE, defPtSiz = 1.25, theme = "flatly", tabs = c("civge", "civci", "gevge", "gem", "gec", "vio", "pro", "hea", "about"), font = "Lato", ganalytics = NA) {
  
  tbs <- c("civge", "civci", "gevge", "gem", "gec", "vio", "pro", "hea", "mar", "about")
  if(length(tabs) < 1) stop("At least 1 tab must be specified.")
  if(any(!tabs %in% tbs)) stop(paste("One of more tabs are incorrect. Tab options are:",paste(tbs,collapse=", "),"."))
  
  
  subst <- "#"
  if (enableSubset) {
    subst <- ""
  }
  defPtSiz <- as.character(defPtSiz)
  slibs <- c("shiny", "shinyhelper", "data.table", "Matrix", "DT", "magrittr", "ggplot2", "ggplotify", "ggrepel", "hdf5r", "ggdendro", "grid", "shinycssloaders", "patchwork")
  ulibs <- c("shiny", "shinyhelper", "bslib", "data.table", "Matrix", "DT", "magrittr")
  if(!system.file(package="showtext")=="") ulibs <- c(ulibs,"showtext")

  ### Write code for server.R
  fname <- paste0(shiny.dir, "/server.R")
  write_file(wr_lib(slibs), file = fname)
  write_file(wr_font(font = font), append = TRUE, file = fname)
  write_file(wr_load(shiny.prefix, tabs = tabs), append = TRUE, file = fname)
  write_file(wr_sv_fix(font = font), append = TRUE, file = fname)
  write_file(wr_sv_main(shiny.prefix, subst, font = font, tabs = tabs), append = TRUE, file = fname)
  write_file(wr_sv_end(), append = TRUE, file = fname)

  ### Write code for ui.R
  fname <- paste0(shiny.dir, "/ui.R")
  write_file(wr_lib(ulibs), file = fname)
  write_file(wr_load(shiny.prefix, tabs = tabs), append = TRUE, file = fname)
  write_file(wr_ui_single(shiny.title, theme = theme, ganalytics = ganalytics), append = TRUE, file = fname)
  write_file(wr_ui_main(shiny.prefix, subst, defPtSiz, tabs = tabs), append = TRUE, file = fname)
  ## write_file('\n)\n', append = TRUE, file = fname)
  write_file(wr_ui_end(), append = TRUE, file = fname)

  ### Write code for google-analytics.html
  if (!is.na(ganalytics)) {
    fname <- paste0(shiny.dir, "/google-analytics.html")
    write_file(wr_ui_ga(ganalytics), file = fname)
  }

  ### Write extra css
  if (!dir.exists(file.path(shiny.dir, "www"))) dir.create(path = file.path(shiny.dir, "www"))
  path_css <- file.path(shiny.dir, "www", "styles.css")
  if (!file.exists(path_css)) file.create(path_css)
  write_file(wr_css(), file = path_css)

  ### Write about
  if ("about" %in% tabs) {
    path_about <- file.path(shiny.dir, "about.md")
    if (!file.exists(path_about)) file.create(path_about)
    write_file(wr_about(), file = path_about)
  }
}
