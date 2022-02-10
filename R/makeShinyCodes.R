#' Generate code files required for shiny app (one dataset)
#'
#' Generate code files required for shiny app containing only one dataset. In 
#' particular, two R scripts will be generated, namely \code{server.R} and 
#' \code{ui.R}. If users want to include multiple dataset in one shiny app, 
#' please use \code{makeShinyCodesMulti()} instead. Note that both 
#' \code{makeShinyFiles} and \code{makeShinyCodes} functions are ran when 
#' running the wrapper function \code{makeShinyApp}.
#'
#' @param shiny.title title for shiny app
#' @param shiny.prefix specify file prefix 
#' @param shiny.dir specify directory to create the shiny app in
#' @param enableSubset specify whether to enable "Toggle to subset cells" 
#'   functionality in the shiny app. Default is to enable this functionality
#' @param defPtSiz specify default point size for single cells. For example, a 
#'   smaller size can be used if you have many cells in your dataset
#' @param theme Bootsrap theme
#' @param tabs Vector of tab numbers to include
#' @param about Should about page be added as a tab?
#' @param ganalytics Google analytics tracking ID (e.g. "UA-123456789-0")
#'
#' @return server.R and ui.R required for shiny app
#'
#' @author John F. Ouyang
#'
#' @import data.table readr glue
#' @importFrom utils packageVersion
#'
#' @export
makeShinyCodes <- function(shiny.title, shiny.prefix, shiny.dir, 
                           enableSubset = TRUE, defPtSiz = 1.25,
                           theme = "flatly",
                           tabs = c(1,2,3,4,5,6,7),
                           about = TRUE,
                           ganalytics = NA){
  subst = "#"
  if(enableSubset){subst = ""}
  defPtSiz = as.character(defPtSiz)
  
  if(packageVersion("readr") >= "1.4.0"){
    ### Write code for server.R
    fname = paste0(shiny.dir, "/server.R")
    readr::write_file(wrLib(
      c("shiny","shinyhelper","data.table","Matrix","DT","magrittr","ggplot2",
        "ggrepel","hdf5r","ggdendro","gridExtra")), file = fname)
    readr::write_file(wrSVload(shiny.prefix), append = TRUE, file = fname)
    readr::write_file(wrSVfix(), append = TRUE, file = fname)
    readr::write_file(wrSVmain(shiny.prefix, subst, tabs = tabs), append = TRUE, file = fname)
    readr::write_file(wrSVend(), append = TRUE, file = fname)
    
    
    ### Write code for ui.R
    fname = paste0(shiny.dir, "/ui.R")
    readr::write_file(wrLib(
      c("shiny","shinyhelper","shinythemes","data.table","Matrix","DT","magrittr")), file = fname)
    readr::write_file(wrUIload(shiny.prefix), append = TRUE, file = fname)
    readr::write_file(wrUIsingle(shiny.title, theme = theme, ganalytics = ganalytics), append = TRUE, file = fname)
    readr::write_file(wrUImain(shiny.prefix, subst, defPtSiz, tabs = tabs, about = about), append = TRUE, file = fname)
    ##readr::write_file('\n)\n', append = TRUE, file = fname)
    readr::write_file(wrUIend(), append = TRUE, file = fname)
    
    ### Write code for google-analytics.html
    if(!is.na(ganalytics)){
      fname = paste0(shiny.dir, "/google-analytics.html")
      readr::write_file(wrUIga(ganalytics), file = fname)
    }

  } else {
    ### Write code for server.R
    fname = paste0(shiny.dir, "/server.R")
    readr::write_file(wrLib(
      c("shiny","shinyhelper","data.table","Matrix","DT","magrittr","ggplot2",
        "ggrepel","hdf5r","ggdendro","gridExtra")), file = fname)
    readr::write_file(wrSVload(shiny.prefix), append = TRUE, path = fname)
    readr::write_file(wrSVfix(), append = TRUE, file = fname)
    readr::write_file(wrSVmain(shiny.prefix, subst, tabs = tabs), append = TRUE, path = fname)
    readr::write_file(wrSVend(), append = TRUE, file = fname)
    
    
    ### Write code for ui.R
    fname = paste0(shiny.dir, "/ui.R")
    readr::write_file(wrLib(
      c("shiny","shinyhelper","shinythemes","data.table","Matrix","DT","magrittr")), path = fname)
    readr::write_file(wrUIload(shiny.prefix), append = TRUE, file = fname)
    readr::write_file(wrUIsingle(shiny.title, ganalytics), append = TRUE, file = fname)
    readr::write_file(wrUImain(shiny.prefix, subst, defPtSiz, tabs = tabs, about = about), append = TRUE, file = fname)
    ##readr::write_file('\n)\n', append = TRUE, file = fname)
    readr::write_file(wrUIend(), append = TRUE, file = fname)
    
    
    ### Write code for google-analytics.html
    if(!is.na(ganalytics)){
      fname = paste0(shiny.dir, "/google-analytics.html")
      readr::write_file(wrUIga(ganalytics), file = fname)
    }
  }
  
  ### Write extra css
  if(!dir.exists(file.path(shiny.dir,"www"))) dir.create(path=file.path(shiny.dir,"www"))
  path_css <- file.path(shiny.dir,"www","styles.css")
  if(!file.exists(file.path(shiny.dir,"www","styles.css"))) file.create(path_css)
  readr::write_file(wrCSS(), file = path_css)
  
  ### Write about
  if(about){
    if(!file.exists(file.path(shiny.dir,"about.md"))) file.create(file.path(shiny.dir,"about.md"))
  }
}


