#' Checks whether the model's fitted values make sense
#'  
#' @param ddf.model ddf object
#' @return boolean
#' @author Laura Marshall
#'
check.fitted <- function(ddf.model){
  #Fitted values of 0 do not make sense and cause errors in dht
  #They result in a Pa of 0 and N of Inf
  if(any(ddf.model$fitted <= 0)){
    #There is a problem
    return(FALSE)
  }else{
    #Everything seems ok
    return(TRUE)
  }
}