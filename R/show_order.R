#' @title Shows the order in which metadata will be displayed
#' @description Shows the order in which metadata will be displayed in the shiny app. This 
#' helps users to decide if the display order is ok. If not, users can use 
#' \code{reorder_meta} to change the order in which metadata will be displayed.
#' @param scConf shinycell config data.table
#' @return table showing the order in which metadata will be displayed
#' @author John F. Ouyang
#' @author Roy Francis
#' @import data.table
#' @examples
#' \dontrun{
#' show_order(scConf)
#' }
#' @export
#' 
show_order <- function(scConf){
  
  # Start!
  ggOut = scConf[, c("ID", "UI", "fID"), with = FALSE]
  ggOut$nlvl = 0
  ggOut$default = as.character(scConf$default)
  for(i in seq_along(ggOut$ID)){
    ggOut[i]$nlvl = length(strsplit(ggOut[i]$fID, "\\|")[[1]])
  }
  ggOut[is.na(fID)]$nlvl = 0
  ggOut[is.na(fID)]$fID = "cont."
  ggOut[nlvl > 0]$fID = "cat."
  ggOut[default == 0]$default = ""
  colnames(ggOut) = c("actual name", "display name", "type", "nlevels", "default")

  return(ggOut)
}


