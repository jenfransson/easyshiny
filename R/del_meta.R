#' @title Remove a metadata from being included in the shiny app
#' @description Remove a metadata from being included in the shiny app.
#' @param scConf shinycell config data.table
#' @param m metadata to delete. Users can either use the original 
#'   metadata column names or display names. For more information regarding 
#'   display name, see \code{?modMetaName}. Multiple metadata can be specified.
#' @return updated shinycell config data.table
#' @author John F. Ouyang
#' @author Roy Francis
#' @import data.table
#' @examples
#' \dontrun{
#' scConf = del_meta(scConf, c("orig.ident"))
#' }
#' @export
#' 
del_meta <- function(scConf, m){
  # Check if m exist
  if(all(m %in% scConf$ID)){
    useID = TRUE   # Use IDs
  } else if(all(m %in% scConf$UI)){
    useID = FALSE  # Use UIs
  } else {
    stop("m not found in shinycell config!")
  }
  
  # Start removing meta.data
  if(useID){
    scConf = scConf[!ID %in% m]
  } else {
    scConf = scConf[!UI %in% m]
  }
  
  # Reassign default if it is removed
  if(!1 %in% scConf$default){
    chkname = paste0(scConf[default != 2]$ID, "_", scConf[default != 2]$UI)
    def1 = scConf[default != 2]$ID[grep("ident|library", chkname, 
                                        ignore.case = TRUE)[1]]
    if(is.na(def1)){def1 = scConf[default != 2]$ID[1]}
    scConf[ID == def1]$default = 1
  }
  if(!2 %in% scConf$default){
    chkname = paste0(scConf[default != 1]$ID, "_", scConf[default != 1]$UI)
    def2 = scConf[default != 1]$ID[grep("clust", chkname, 
                                        ignore.case = TRUE)[1]]
    if(is.na(def2)){def2 = scConf[default != 1]$ID[1]}
    scConf[ID == def2]$default = 2
  }
  
  return(scConf)
}


