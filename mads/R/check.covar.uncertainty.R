#' Performs checks on the covariate.uncertainty dataframe 
#'
#' Ensures that where necessary the values are characters and that only 
#' supported sampling distributions have been selected.
#'  
#' @param covariate.uncertainty dataframe containing information used to 
#'   parametrically resample data or NULL if not required
#' @return verified covariate.uncertainty dataframe 
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#' @seealso \code{execute.multi.analysis}
#' @keywords input validation, data validation
#'
#'
check.covar.uncertainty <- function(covariate.uncertainty){
# check.covar.uncertainty function to perform checks on the covariate.uncertainty dataframe
#
# Arguments:  
#  covariate.uncertainty - dataframe containing information used to parametrically resample data   
#
# Value:  
#  returns the updated covariate.uncertainty dataframe
#
# Function Calls: none
# 
  if(is.null(covariate.uncertainty)){
    return(NULL)
  }                                   
  #Make sure these options are all characters
  for(i in c(1,2,4:7)){
    covariate.uncertainty[,i] <- as.character(covariate.uncertainty[,i])
  }
  
  #Check that chosen variables exists                                           #NEEDS TO BE ADDED - otherwise object ID is selected
  
  #compare chosen values with those allowed
  compare <- covariate.uncertainty$sampling.distribution%in%c("Normal", "Normal.Absolute", "Lognormal.BC", "Poisson", "TruncPoisson.BC")
  if(length(which(!compare)) != 0){
    process.warnings()
    stop(paste("An unsupported sampling distribution has been chosen for covariate uncertainty. Only one of the following may be specified: Normal, Normal.Absolute, Lognormal.BC, Poisson, TruncPoisson.BC",sep = ""), call. = FALSE)
  }
  
  #Make sure variables exist in the dataframes.
  #for(i in seq(along = covariate.uncertainty$variable.layer)){
  #  if(is.null(observations) | is.null(uncertainty)){
  #      process.warnings()
  #      stop("Invalid names for the covariates or associated uncertainty have been specified in the covariate uncertainty dataframe.")
  #    
  #  }
  #}
  
  #vector of implemented sampling distributions
  #sampling.distributions <- c("Normal", "Normal.Absolute", "Lognormal.BC", "Poisson", "TruncPoisson.BC")
  
  #make sure that only these distributions have been selected
  #for (i in seq(along = covariate.uncertainty$sampling.distribution)){
    #if there is no match in the vector of implemented distributions
  #  if(is.na(match(covariate.uncertainty$sampling.distribution[i], sampling.distributions))){
      #give an error
  #    stop(paste("An unsupported sampling distribution has been chosen for covariate number ",i, ". Only one of the following may be specified: Normal, Normal.Absolute, Lognormal.BC, Poisson, TruncPoisson.BC",sep = ""))
  #  } 
  #}
  
  return(covariate.uncertainty)
}
