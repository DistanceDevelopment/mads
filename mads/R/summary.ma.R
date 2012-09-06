#' Summary of distance detection function model object
#' 
#' Provides a brief summary of data and fitted detection probability model
#' parameters, model selection criterion, and optionally abundance in the
#' covered (sampled) region and its standard error.
#' 
#' The corresponding print function is called to print the summary results.
#' 
#' @S3method summary ma
#' @method summary ma
#' @aliases summary.ma
#' @param object a \code{ma} model object
#' @param \dots unspecified and unused arguments for S3 consistency
#' @return list of extracted and summarized objects
#' @note This function is called by the generic function \code{summary} for any
#'   \code{ma} object.  
#' @author Laura Marshall
#' @keywords utility
summary.ma <- function(object,...){
  cat("\nMulti-Analysis Summary \n")
  species.name <- names(object)   
  for(sp in seq(along = species.name)){
    summary(object[[sp]], species = species.name[sp])
  }     
  #for(sp in seq(along = object)){
    #class(object[[sp]]) <- "ma.element"
  #}
  #class(object) <- "summary.ma"
  invisible(object)
}