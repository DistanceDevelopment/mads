#' Creates a global object named MAE.warnings
#'
#' This object will be used to store all warning messages generated while the
#' bootstrap is being run. This ensures that a summary of all warning messages
#' are available to the user on completion. If an object called MAE.warnings
#' already exists in the workspace the user is asked to remove this object
#' begore running the multi-analysis again.
#'  
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#' @seealso \code{\link{process.warnings}}
#' @keywords utility
#'
create.warning.storage <- function(){
#
# create.warning.storage - function to create a global object named MAE.warnings
#
# Arguments      - none
# Value          - none
# Function calls - none
#
  #get objects starting with "MAE.warnings"
  MAE.warnings.objects <- ls(1)[grep("MAE.warnings", ls(1))]
  #if MAE.warnings object does not exists create it
  if(is.na(match("MAE.warnings", MAE.warnings.objects))){
    MAE.warnings <<- NULL 
  }else{
    #otherwise warn the user to remove MAE.warnings and run the analyses again
    stop(paste("The MA engine needs to create a global object named MAE.warnings, however, one already exists in the workspace. \nPlease rename this object before re-running the analyses."), call. = FALSE)
  }
}



