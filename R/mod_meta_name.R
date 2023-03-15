#' @title Modify the display name of metadata
#' @description Modify the display name of metadata. It is possible that the original 
#' metadata name is not so informative e.g "orig.ident" or too long e.g. 
#' "seurat_clusters" and users want to shorten the way they are displayed 
#' on the shiny app. This function allows users to specify display names for 
#' metadata i.e. the names that will be displayed on the shiny app. Note that 
#' \code{show_legend} shows the display name instead of the actual name.
#' @param scConf shinycell config data.table
#' @param m metadata for which to modify the display name. Users can 
#'   either use the actual metadata column names or display names. Multiple 
#'   metadata can be specified. It is recommended to use the original metadata 
#'   column names to reduce confusion.
#' @param nn new display names for the corresponding metadata
#' @return updated shinycell config data.table
#' @author John F. Ouyang
#' @author Roy Francis
#' @import data.table
#' @examples
#' \dontrun{
#' scConf = mod_meta_name(scConf, 
#'                      m = c("orig.ident", "seurat_clusters"), 
#'                      nn = c("library", "cluster"))
#' }
#' @export
#' 
mod_meta_name <- function(scConf, m, nn){
  # Check if m exist
  if(all(m %in% scConf$ID)){
    useID = TRUE   # Use IDs
  } else if(all(m %in% scConf$UI)){
    useID = FALSE  # Use UIs
  } else {
    stop("m not found in shinycell config!")
  }
  
  # Check replacement length
  if(length(m) != length(nn)){
    stop("Lengths of m and nn do not match!")
  }
  
  # Start changing display name
  if(useID){
    for(i in seq_along(m)){
      scConf[ID == m[i]]$UI = nn[i]
    }
  } else {
    for(i in seq_along(m)){
      scConf[UI == m[i]]$UI = nn[i]
    }
  }
  
  return(scConf)
}


