#' Summarises warnings
#'
#' Sumarises warnings generated during the bootstrap and removes the 
#' MAE.warnings global object.  
#'  
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#'
process.warnings <- function(){
# process.warnings function to summarise warnings
#
# Arguments: none
# Value: none
# Function Calls: none
#
  mae.warning <- unique(MAE.warnings)
  no.warnings <- NULL
  for(i in seq(along = mae.warning)){
    no.warnings <- length(which(MAE.warnings == mae.warning[i])) 
    cat(mae.warning[i], " [warning occured ",no.warnings," times]", sep = "", fill = TRUE) 
  }
  rm(MAE.warnings, pos=1)
}  


