#' Refits the detection functions to the resampled data
#'
#' Fits all the models named in ddf.models to the associated data supplied in 
#' ddf.dat.working. If more than one model is supplied for any species the 
#' model with the minimum selection crieteria will be selected.
#'  
#' @param ddf.dat.working list of dataframes containing the data to which the 
#'   models will be fitted
#' @param ddf.models list of unique character vectors giving the names of the 
#'   ddf objects for each species.
#' @param criterion character option specifying the model selection criteria - 
#'   "AIC", "AICc" or "BIC".
#' @return list of ddf objects 
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#'
fit.ddf.models <- function(ddf.dat.working, ddf.models, criterion){
# fit.ddf.models function to refits the detection functions to the data provided
#
# Arguments:
#   ddf.dat.working list of dataframes containing the datasets
#   ddf.models list of unique character vectors specifying the model names
#   criterion character model selection option ("AIC", "AICc" or "BIC")
#
# Value: list of ddf objects 
#
# Function Calls: mrds::ddf
#
  species.name <- names(ddf.dat.working)
  #create storage list
  ddf.results <- list()
  #for every species code
  for(sp in seq(along = species.name)){
    temp.results <- list()
    selection.criterion.values <- NULL
    #get dataset
    usedata <- ddf.dat.working[[species.name[sp]]]
    #for every model
    for(m in seq(along = ddf.models[[species.name[sp]]])){
      #get model name
      model.name <- ddf.models[[species.name[sp]]][m]   
      #get model call                        
      model.call <- get(model.name)$call
      #point the call at the new data
      model.call$data <- as.name("usedata")
      #get parameter estimates from original model to act as start values to aid convergence
      start.values <- list(scale=get(model.name)$ds$aux$ddfob$scale$parameters, shape=get(model.name)$ds$aux$ddfob$shape$parameters, adjustment=get(model.name)$ds$aux$ddfob$adjustment$parameters)                           
      model.call$control <- call("list",initial=start.values)
      #refit ddf model
      temp.results[[m]] <- try(eval(model.call))
      if(class(temp.results[[m]])[1] == "try-error"){
        cat("Model did not converge for species ",species.name[sp]," model ",model.name, sep="", fill=TRUE)
      }else{
        cat("Model converged for species ",species.name[sp]," model ",model.name, sep="", fill=TRUE)
        lnl <- temp.results[[m]]$lnl 
        k <- length(temp.results[[m]]$par)
        n <- nrow(temp.results[[m]]$data)
        selection.criterion.values[m] <- switch(criterion,
          AIC  = 2*k-2*lnl,
          AICc = 2*k-2*lnl+(2*k*(k+1))/(n-k-1),
          BIC  = k*log(n)-2*lnl)             
      }
    } 
  #Find the model with the minimum selection criteria
  if(!is.null(selection.criterion.values)){  
    selected.model <- which(selection.criterion.values == min(selection.criterion.values))  #if 2 models have the same selection criteria how should we choose between them?
    ddf.results[[species.name[sp]]] <- temp.results[[selected.model[1]]]
  }else{
    ddf.results[[species.name[sp]]] <- NULL
  }
  }
  return(ddf.results)
}



