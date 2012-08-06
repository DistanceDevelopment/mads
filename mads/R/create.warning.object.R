#' Checks the list of model names supplied by the user
#'
#' Performs various checks on the model names supplied by the user. If the
#' multi-analysis engine is being called from Distance then the model names are
#' transformed from numbers to names. This also checks that the data in each
#' model are the same across species codes to ensure valid comparison using the
#' AIC/AICc/BIC selection criteria.
#'  
#' @param ddf.models
#' @param species.name
#' @param dist.names
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#'
create.warning.storage <- function(){
  #get objects starting with "MAE.warnings"
  MAE.warnings.objects <- ls(1)[grep("MAE.warnings", ls(1))]
  #if none exist create one
  if(is.na(match("MAE.warnings", MAE.warnings.objects))){
    MAE.warnings <<- NULL 
  }else{
    stop(paste("The MA engine needs to create a global object named MAE.warnings, however, one already exists in the workspace. \nPlease rename this object before re-running the analyses."), call. = FALSE)
  }
}



