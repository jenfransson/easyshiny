#' Generate code files required for shiny app (multi datasets)
#'
#' Generate code files required for shiny app containing multiple datasets. In
#' particular, two R scripts will be generated, namely \code{server.R} and
#' \code{ui.R}. Note that \code{makeShinyFiles} has to be ran prior to
#' generate the necessary data files for each of the dataset to be included.
#' The prefix used in \code{makeShinyFiles} have to be then supplied in this
#' function.
#'
#' @param shiny.title specify the overall title for shiny app
#' @param shiny.prefix specify file prefix for each dataset. Must match the
#'   prefix used in \code{makeShinyFiles}
#' @param shiny.headers specify the tab header names for each dataset. Length
#'   must match that of \code{shiny.prefix}
#' @param shiny.dir specify directory to create the shiny app in
#' @param enableSubset specify whether to enable "Toggle to subset cells"
#'   functionality in the shiny app. Default is to enable this functionality
#' @param defPtSiz specify default point size for single cells. For example, a
#'   smaller size can be used if you have many cells in your dataset. A single
#'   value can be specified to set the point size for all datasets. Otherwise,
#'   users have to specify one value for each dataset
#' @param theme Bootsrap theme
#' @param tabs Vector of tab numbers to include
#' @param about Should about page be added as a tab?
#' @param ganalytics Google analytics tracking ID (e.g. "UA-123456789-0")
#' @param extra_css Logical indication if additional CSS is to be created.
#'
#' @return server.R and ui.R required for shiny app
#'
#' @author John F. Ouyang
#'
#' @import data.table readr
#' @importFrom utils packageVersion
#'
#' @export
makeShinyCodesMulti <- function(shiny.title, shiny.prefix, shiny.headers, shiny.dir,
                                enableSubset = TRUE, defPtSiz = 1.25,
                                theme = "flatly",
                                tabs = c(1, 2, 3, 4, 5, 6, 7),
                                about = TRUE,
                                ganalytics = NA,
                                extra_css = FALSE) {
  ### Checks
  if (length(shiny.prefix) != length(shiny.headers)) {
    stop("length of shiny.prefix and shiny.headers does not match!")
  }
  subst <- "#"
  if (enableSubset) {
    subst <- ""
  }
  if (length(shiny.prefix) != length(defPtSiz)) {
    defPtSiz <- rep(defPtSiz[1], length(shiny.prefix))
  }
  defPtSiz <- as.character(defPtSiz)

  if (packageVersion("readr") >= "1.4.0") {
    ### Write code for server.R
    fname <- paste0(shiny.dir, "/server.R")
    readr::write_file(wrLib(c("shiny", "shinyhelper", "data.table", "Matrix", "DT", "magrittr", "ggplot2", "ggrepel", "hdf5r", "ggdendro", "gridExtra")), file = fname)
    for (i in shiny.prefix) {
      readr::write_file(wrSVload(i), append = TRUE, file = fname)
    }
    readr::write_file(wrSVfix(), append = TRUE, file = fname)
    for (i in shiny.prefix) {
      readr::write_file(wrSVmain(i, subst, tabs = tabs), append = TRUE, file = fname)
    }
    readr::write_file(wrSVend(), append = TRUE, file = fname)


    ### Write code for ui.R
    fname <- paste0(shiny.dir, "/ui.R")
    readr::write_file(wrLib(
      c("shiny", "shinyhelper", "shinythemes", "data.table", "Matrix", "DT", "magrittr")
    ), file = fname)
    for (i in shiny.prefix) {
      readr::write_file(wrUIload(i), append = TRUE, file = fname)
    }
    readr::write_file(wrUIsingle(shiny.title, theme = theme, ganalytics = ganalytics, extra_css = extra_css), append = TRUE, file = fname)
    for (i in seq_along(shiny.prefix)) {
      hhh <- shiny.headers[i]
      readr::write_file(glue::glue('\n\n,navbarMenu("{hhh}"'), append = TRUE, file = fname)
      readr::write_file(wrUImain(shiny.prefix[i], subst, defPtSiz[i], tabs = tabs, about = FALSE), append = TRUE, file = fname)
      readr::write_file('\n)', append = TRUE, file = fname)
    }
    if(about){
      readr::write_file(glue::glue('\n\n,navbarMenu("About"'), append = TRUE, file = fname)
      readr::write_file(wrUIabout(), append = TRUE, file = fname)
      readr::write_file('\n)', append = TRUE, file = fname)
    }
    readr::write_file(wrUIend(), append = TRUE, file = fname)


    ### Write code for google-analytics.html
    if (!is.na(ganalytics)) {
      fname <- paste0(shiny.dir, "/google-analytics.html")
      readr::write_file(wrUIga(ganalytics), file = fname)
    }
  } else {
    ### Write code for server.R
    fname <- paste0(shiny.dir, "/server.R")
    readr::write_file(wrLib(
      c("shiny", "shinyhelper", "data.table", "Matrix", "DT", "magrittr", "ggplot2", "ggrepel", "hdf5r", "ggdendro", "gridExtra")
    ), path = fname)
    for (i in shiny.prefix) {
      readr::write_file(wrSVload(i), append = TRUE, path = fname)
    }
    readr::write_file(wrSVfix(), append = TRUE, path = fname)
    for (i in shiny.prefix) {
      readr::write_file(wrSVmain(i, subst, tabs = tabs), append = TRUE, path = fname)
    }
    readr::write_file(wrSVend(), append = TRUE, path = fname)


    ### Write code for ui.R
    fname <- paste0(shiny.dir, "/ui.R")
    readr::write_file(wrLib(
      c("shiny", "shinyhelper", "shinythemes", "data.table", "Matrix", "DT", "magrittr")
    ), path = fname)
    for (i in shiny.prefix) {
      readr::write_file(wrUIload(i), append = TRUE, path = fname)
    }
    readr::write_file(wrUIsingle(shiny.title, theme = theme, ganalytics = ganalytics, extra_css = extra_css), append = TRUE, path = fname)
    for (i in seq_along(shiny.prefix)) {
      hhh <- shiny.headers[i]
      readr::write_file(glue::glue('\n\n,navbarMenu("{hhh}"'), append = TRUE, path = fname)
      readr::write_file(wrUImain(shiny.prefix[i], subst, defPtSiz[i], tabs = tabs, about = FALSE), append = TRUE, path = fname)
      readr::write_file('\n)', append = TRUE, file = fname)
    }
    if(about){
      readr::write_file(glue::glue('\n\n,navbarMenu("About"'), append = TRUE, file = fname)
      readr::write_file(wrUIabout(), append = TRUE, file = fname)
      readr::write_file('\n)', append = TRUE, file = fname)
    }
    readr::write_file(wrUIend(), append = TRUE, path = fname)

    ### Write code for google-analytics.html
    if (!is.na(ganalytics)) {
      fname <- paste0(shiny.dir, "/google-analytics.html")
      readr::write_file(wrUIga(ganalytics), file = fname)
    }
  }

  ### Write extra css
  if (extra_css) {
    if (!dir.exists(file.path(shiny.dir, "www"))) dir.create(path = file.path(shiny.dir, "www"))
    path_css <- file.path(shiny.dir,"www","styles.css")
    if(!file.exists(file.path(shiny.dir,"www","styles.css"))) file.create(path_css)
    readr::write_file(wrCSS(), file = path_css)
  }

  ### Write about
  if (about) {
    if (!file.exists(file.path(shiny.dir, "about.md"))) file.create(file.path(shiny.dir, "about.md"))
  }
}
