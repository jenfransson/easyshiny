#' Modify the colour palette for categorical metadata
#'
#' Modify the colour palette for categorical metadata.
#'
#' @param scConf shinycell config data.table
#' @param m metadata for which to modify the colour palette. Users 
#'   can either use the actual metadata column names or display names. Please 
#'   specify only one metadata
#' @param nc character vector of new colour palette
#' 
#' @return updated shinycell config data.table
#'
#' @author John F. Ouyang
#'
#' @import data.table
#' @importFrom grDevices col2rgb
#'
#' @examples
#' \dontrun{
#' scConf = mod_colour(scConf, m = "library", nc = c("black", "darkorange", "blue", "red"))
#'}
#'
#' @export
mod_colour <- function(scConf, m, nc){
  # Check that only one metadata is provided
  if(length(m) != 1){
    stop("Please specify only one metadata to modify colour palette!")
  }
  
  # Check if m exist
  if(m %in% scConf$ID){
    useID = TRUE   # Use IDs
  } else if(m %in% scConf$UI){
    useID = FALSE  # Use UIs
  } else {
    stop("m not found in shinycell config!")
  }
  
  # Check if nc are valid colours
  res = try(col2rgb(nc), silent = TRUE)
  if("try-error" %in% class(res)){
    stop("invalid colours are provided!")
  }
  
  # Check if m is categorical and if length(nc) matches 
  if(useID){
    res = strsplit(scConf[ID == m]$fCL, "\\|")[[1]]
  } else {
    res = strsplit(scConf[UI == m]$fCL, "\\|")[[1]]
  }
  if(is.na(res[1])){
    stop("m is not a categorical metadata!")
  }
  if(length(res) != length(nc)){
    stop("Length of nc does not match!")
  }
  
  # Start changing the colours
  if(useID){
    scConf[ID == m]$fCL = paste0(nc, collapse = "|")
  } else {
    scConf[UI == m]$fCL = paste0(nc, collapse = "|")
  }
  return(scConf)
}


