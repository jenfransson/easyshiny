#' @title Modify the legend labels for categorical metadata
#' @description Modify the legend labels for categorical metadata.
#' @param scConf shinycell config data.table
#' @param m metadata for which to modify the legend labels. Users 
#'   can either use the actual metadata column names or display names. Please 
#'   specify only one metadata
#' @param new.labels character vector of new legend labels
#' @return updated shinycell config data.table
#' @author John F. Ouyang
#' @author Roy Francis
#' @import data.table
#' @examples
#' \dontrun{
#' scConf = mod_label(scConf, m = "library", new.labels = c("Fib", "Primed", "Naive", "RSeT"))
#'}
#' @export
#' 
mod_label <- function(scConf, m, new.labels){
  # Check that only one metadata is provided
  if(length(m) != 1){
    stop("Please specify only one metadata to modify legend labels!")
  }
  
  # Check if m exist
  if(m %in% scConf$ID){
    useID = TRUE   # Use IDs
  } else if(m %in% scConf$UI){
    useID = FALSE  # Use UIs
  } else {
    stop("m not found in shinycell config!")
  }
  
  # Check if m is categorical and if length(new.labels) matches 
  if(useID){
    res = strsplit(scConf[ID == m]$fUI, "\\|")[[1]]
  } else {
    res = strsplit(scConf[UI == m]$fUI, "\\|")[[1]]
  }
  if(is.na(res[1])){
    stop("m is not a categorical metadata!")
  }
  if(length(res) != length(new.labels)){
    stop("Length of new.labels does not match!")
  }
  
  # Start changing the colours
  if(useID){
    scConf[ID == m]$fUI = paste0(new.labels, collapse = "|")
  } else {
    scConf[UI == m]$fUI = paste0(new.labels, collapse = "|")
  }
  return(scConf)
}


