#' @title Reorder the order in which metadata appear in the shiny app
#' @description Reorder the order in which metadata appear in the dropdown menu in the 
#' shiny app.
#' @param scConf shinycell config data.table
#' @param nmo character vector containing new order. All metadata 
#'   names must be included, which can be found at \code{scConf$ID}
#' @return updated shinycell config data.table
#' @author John F. Ouyang
#' @author Roy Francis
#' @import data.table
#' @examples
#' \dontrun{
#' scConf = reorder_meta(scConf, scConf$ID[c(1,3,2,4:length(scConf$ID))])
#' }
#' @export
#' 
reorder_meta <- function(scConf, nmo){
  # Check if nmo matches scConf$ID
  if(!all.equal(sort(nmo), sort(as.character(scConf$ID)))){
    stop("nmo does not match scConf$ID!")
  }
  
  # Start reordering
  scConf$ID = factor(scConf$ID, levels = nmo)
  scConf = scConf[order(ID)]

  return(scConf)
}


