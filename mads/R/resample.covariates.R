#' Parametrcally resamples selected variables in the dataset.
#'
#' Parametricelly resamples specified variables in the dataset based on a 
#' measure of uncertainty and a sampling distribution supplied by the user.
#'  
#' @param ddf.dat.working a list of datasets to be used in the analyses
#' @param covariate.uncertainty a dataframe detailing the variables to be 
#'   resampled - variable.layer, variable.name, correction.factor.layer,        #THIS FUNCTION NEEDS TO BE MODIFIED TO TAKE IN CORRECTION FACTOR LAYER TOO!
#'   variable.correction.factor , uncertainty.layer, uncertainty.name, 
#'   uncertainty.measure, sampling.distribution.
#' @return list of dataframes containing the parametrically resampled data
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#' @keywords data manipulation
#'         
resample.covariates <- function(ddf.dat.working, covariate.uncertainty){
# adds uncertainty and/or applies correction factors to the specified covariates 
# currently only implemented for covariates in the observation layer            #IMPLEMENT FOR OTHER DATA LAYERS
#
# Arguments:
#   ddf.dat.working - a list of dataframes containing the datasets
#   covariate.uncertainty - dataframe detailing how the variables are to be resampled
#
# Value:
#   list of dataframes containing the resampled data
#
# Functions Used: None 
#
  species.name <- names(ddf.dat.working)

  #for every covariate with uncertainty
  for(covar in seq(along = covariate.uncertainty$variable.name)){
  
    #for every species
    for(sp in seq(along = species.name)){
    
      #Obtain observations and estimated errors
      #eval(parse(text=paste("observations <- ddf.dat.working$",species.name[sp],"$",covariate.uncertainty$variable.name[covar],sep="")))
      #eval(parse(text=paste("uncertainty <-  ddf.dat.working$",species.name[sp],"$",covariate.uncertainty$uncertainty.name[covar],sep="")))
      observations <- ddf.dat.working[[species.name[sp]]][[covariate.uncertainty$variable.name[covar]]]
      uncertainty <- ddf.dat.working[[species.name[sp]]][[covariate.uncertainty$uncertainty.name[covar]]]
      
      #check that both the observations and uncertainty exist
      if(is.null(observations) | is.null(uncertainty)){
        process.warnings()
        stop("Invalid names for the covariates or associated uncertainty have been specified in the covariate uncertainty dataframe.")
      }
                                                                                # what this varies across observer?
      #Apply correction factor
      observations <- observations*covariate.uncertainty$variable.correction.factor[covar]
      
    
      #Turn uncertainty measure into standard deviation
      error.sd <- switch(covariate.uncertainty$uncertainty.measure[covar],
        sd  = uncertainty,
        var = sqrt(uncertainty),
        CV  = uncertainty*observations)                                         #is this right or should it be uncertainty*mean(observation)?  
            
      #if(covariate.uncertainty$sampling.distribution[covar] == "Normal"){      #replace with switch statement
      #  new.observations <- rnorm(length(observations), observations, error.sd)
      #}else if(covariate.uncertainty$sampling.distribution[covar] == "Lognormal"){
      #  new.observations <- exp(rnorm(length(observations), log(observations), error.sd) - ((error.sd*error.sd)/2))
      #}else if(covariate.uncertainty$sampling.distribution[covar] == "Poisson"){
      #  new.observations <- rpois(length(observations), observations)              #what happens when cluster sizes of zero are generated?
      #}
      
      #Data pre-processing for the bias corrected truncated Poisson
      #if(as.character(covariate.uncertainty$sampling.distribution[covar]) == "Poisson.BC"){
      #  pretruncated.means <- truncated.poisson.table$pretruncated.mean[observations] #can make this just a vector rather than a dataframe
      #  pretruncated.means <- ifelse(is.na(pretruncated.means), observations, pretruncated.means)
      #}                                                                        #MOVED TO rtpois()
      
      #generate uncertainty
      #new options need to be added to vector in check.covar.uncertainty()
      new.observations <- switch(covariate.uncertainty$sampling.distribution[covar],
        Normal                = rnorm(length(observations), observations, error.sd),
        Normal.Absolute       = abs(rnorm(length(observations), observations, error.sd)),
        #Normal.Rounded       = rnorm(length(observations), observations, error.sd),
        Lognormal.BC          = exp(rnorm(length(observations), log(observations), error.sd) - ((error.sd*error.sd)/2)),
        Poisson               = rpois(length(observations), observations),
        TruncPoisson.BC       = rtpois(length(observations), observations))
        #Trunc.NegBinomial.BC  = NULL) 
        #wrapped Normal for angles? Von Mises  
      
      #temp<-NULL                                                               #testing to check that bias corrections works
      #for(i in 1:9999){
      #  #new.observations <- exp(rnorm(length(observations), log(observations), error.sd) - ((error.sd*error.sd)/2))
      #  new.observations <- exp(rnorm(length(observations), log(observations), error.sd))
      #  temp[i]<-mean(new.observations)
      #} 
      #hist(temp)
      #abline(v=mean(observations), col=2, lty=2, lwd=2)
      #mean(observations)
      #mean(temp) 
      #median(observations)
      #median(temp)                                                               
      
      
      #check adding uncertainty hasn't caused any observations to be less than zero
      #if so issue appropriate message
      #if not replace the existing observations with the ones generated including error
      observation.check.less.0 <- which(new.observations < 0)
      observation.check.equal.0 <- which(new.observations == 0)
      if(length(observation.check.less.0) > 0 & covariate.uncertainty$variable.name[covar] == "distance"){    #change this to stop it crashing on the 998th iteration
        mae.warning("Parametric resampling has generated distances less than zero", warning.mode="store")  
      }else if(length(observation.check.less.0) > 0 & covariate.uncertainty$variable.name[covar] == "size"){
        mae.warning("Parametric resampling has generated cluster sizes less than zero", warning.mode="store")
      }else if(length(observation.check.equal.0) > 0){
        mae.warning("Parametric resampling has generated cluster sizes of zero", warning.mode="store")  
      }else if(length(observation.check.less.0) > 0){
        mae.warning(paste("Parametric resampling has generated values less than zero for ", covariate.uncertainty$variable.name[covar], sep = ""), warning.mode="store")
      }#else{
      #  eval(parse(text=paste("ddf.dat.working$",species.name[sp],"$",covariate.uncertainty$variable.name[covar]," <- new.observations", sep = "")))
      #}   
      
      #replace original observations with new ones
      #eval(parse(text=paste("ddf.dat.working$",species.name[sp],"$",covariate.uncertainty$variable.name[covar]," <- new.observations",sep = "")))
      ddf.dat.working[[species.name[sp]]][[covariate.uncertainty$variable.name[covar]]] <- new.observations
       
    }#next species    
  }#next covariate
  
  return(ddf.dat.working)  
}