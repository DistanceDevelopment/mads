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
rtpois <- function(N, mean=NA, lambda=NA){
  #find corresponding lambda value for desired mean
  lambda <- truncated.poisson.table$lambda[mean] #can make this just a vector rather than a dataframe
  lambda <- ifelse(is.na(lambda), mean, lambda)
  #generate quantiles from a uniform distribution between the probability of getting a zero, given lambda, and 1.
  #endpoints are altered to ensure p is between these values and not equal to as this generates 0's or Infs.
  p <- runif(N, dpois(0, lambda)+1e-10, 1-1e-10)
  #find the smallest integer x such that P(X <= x) >= p
  tpois <- qpois(p, lambda)
  return(tpois)
}

#THIS NEEDS MODIFYING SO THAT THE OBJECTS AREN'T GLOBAL

rtpois.example <- function(N, lambda){
  p <- runif(N, dpois(0, lambda)+1e-10, 1-1e-10)
  tpois <- qpois(p, lambda)
  return(tpois)
}

#Find the values of the untruncated means which give the required truncated mean
find.pretruncated.mean<-function(x,y){
  (y-(x/(1-exp(-x))))^2
}

pretruncated.mean <- NULL
for(i in 1:20){
  pretruncated.mean[i] <- optimise(f=find.pretruncated.mean,interval=c(0,300), y=i)$minimum
  #cat(pretruncated.mean[i],", ", sep="")
  #cat(pretruncated.mean[i]/(1-exp(-pretruncated.mean[i])),", ",sep="")
  #cat(mean(rtpois.example(1000, pretruncated.mean[i])), fill=TRUE)
}

truncated.poisson.table <- data.frame(mean = 1:20, lambda = pretruncated.mean)
rm(pretruncated.mean, find.pretruncated.mean, rtpois.example)



