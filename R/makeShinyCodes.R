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
#' @param shiny.footnotes text for shiny app footnote. When given as a list, 
#'   citation can be inserted by specifying author, title, journal, volume, 
#'   page, year, doi, link. See example below. 
#' @param shiny.prefix specify file prefix 
#' @param shiny.dir specify directory to create the shiny app in
#' @param enableSubset specify whether to enable "Toggle to subset cells" 
#'   functionality in the shiny app. Default is to enable this functionality
#' @param defPtSiz specify default point size for single cells. For example, a 
#'   smaller size can be used if you have many cells in your dataset
#' @param theme Bootsrap theme
#' @param tabs Vector of tab numbers to include
#' @param ganalytics Google analytics tracking ID (e.g. "UA-123456789-0")
#' @param extra_css Logical indication if additional CSS is to be created.
#'
#' @return server.R and ui.R required for shiny app
#'
#' @author John F. Ouyang
#'
#' @import data.table readr glue
#' @importFrom utils packageVersion
#'
#' @examples
#' \dontrun{
#' # Example citation
#' citation = list(
#'   author  = "Liu X., Ouyang J.F., Rossello F.J. et al.",
#'   title   = "",
#'   journal = "Nature",
#'   volume  = "586",
#'   page    = "101-107",
#'   year    = "2020", 
#'   doi     = "10.1038/s41586-020-2734-6",
#'   link    = "https://www.nature.com/articles/s41586-020-2734-6")
#' makeShinyCodes(shiny.title = "scRNA-seq shiny app", shiny.footnotes = "",
#'                shiny.prefix = "sc1", shiny.dir = "shinyApp/")
#' }
#'
#' @export
makeShinyCodes <- function(shiny.title, shiny.footnotes,
                           shiny.prefix, shiny.dir, 
                           enableSubset = TRUE, defPtSiz = 1.25,
                           theme = "flatly",
                           tabs = c(1,2,3,4,5,6,7),
                           ganalytics = NA,
                           extra_css = FALSE){
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
    readr::write_file(wrUIsingle(shiny.title, theme = theme, ganalytics = ganalytics, extra_css = extra_css), append = TRUE, file = fname)
    readr::write_file(wrUImain(shiny.prefix, subst, defPtSiz, tabs = tabs), append = TRUE, file = fname)
    readr::write_file(wrUIend(shiny.footnotes), append = TRUE, file = fname)
    
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
        "ggrepel","hdf5r","ggdendro","gridExtra")), path = fname)
    readr::write_file(wrSVload(shiny.prefix), append = TRUE, path = fname)
    readr::write_file(wrSVfix(), append = TRUE, path = fname)
    readr::write_file(wrSVmain(shiny.prefix, subst, tabs = tabs), append = TRUE, path = fname)
    readr::write_file(wrSVend(), append = TRUE, path = fname)
    
    
    ### Write code for ui.R
    fname = paste0(shiny.dir, "/ui.R")
    readr::write_file(wrLib(
      c("shiny","shinyhelper","data.table","Matrix","DT","magrittr")), path = fname)
    readr::write_file(wrUIload(shiny.prefix), append = TRUE, path = fname)
    readr::write_file(wrUIsingle(shiny.title, ganalytics, extra_css = extra_css), append = TRUE, path = fname)
    readr::write_file(wrUImain(shiny.prefix, subst, defPtSiz, tabs = tabs), append = TRUE, path = fname)
    readr::write_file(wrUIend(shiny.footnotes), append = TRUE, path = fname)
    
    
    ### Write code for google-analytics.html
    if(!is.na(ganalytics)){
      fname = paste0(shiny.dir, "/google-analytics.html")
      readr::write_file(wrUIga(ganalytics), path = fname)
    }
  }
  
  ### Write extra css
  if(extra_css) {
    if(!dir.exists(file.path(shiny.dir,"www"))) dir.create(path=file.path(shiny.dir,"www"))
    if(!file.exists(file.path(shiny.dir,"www","styles.css"))) file.create(file.path(shiny.dir,"www","styles.css"))
  }
}


