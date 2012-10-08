#' Ramdonly generates values from a zero-truncated Poisson distribution
#'
#' Generates values from a zero-truncated Poisson distribution with mean
#' equal to that specified. It uses a look up table to check which value of 
#' lambda will give values with the requested mean. 
#'  
#' @param N number of values to randomly generate
#' @param mean mean of the generated values
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#'
#' data(truncated.poisson.table)
rtpois <- function(N, mean){
  #find corresponding lambda value for desired mean
  #if(!is.na(mean) & !is.na(lambda)){
  #  warning("Only one of either mean of lambda should be supplied. Mean is being used.")     
  #}
  #if(!is.na(mean)){
  lambda <- truncated.poisson.table$lambda[mean] #can make this just a vector rather than a dataframe
  lambda <- ifelse(is.na(lambda), mean, lambda)
  #}
  #generate quantiles from a uniform distribution between the probability of getting a zero, given lambda, and 1.
  #endpoints are altered to ensure p is between these values and not equal to as this generates 0's or Infs.
  p <- runif(N, dpois(0, lambda)+1e-10, 1-1e-10)
  #find the smallest integer x such that P(X <= x) >= p
  tpois <- qpois(p, lambda)
  return(tpois)
}

#THIS NEEDS MODIFYING SO THAT THE OBJECTS AREN'T GLOBAL

#rtpois.example <- function(N, lambda){
#  p <- runif(N, dpois(0, lambda)+1e-10, 1-1e-10)
#  tpois <- qpois(p, lambda)
#  return(tpois)
#}
#
#Find the values of the untruncated means which give the required truncated mean
#find.pretruncated.mean<-function(x,y){
#  (y-(x/(1-exp(-x))))^2
#}

#pretruncated.mean <- NULL
#for(i in 1:20){
#  pretruncated.mean[i] <- optimise(f=find.pretruncated.mean,interval=c(0,300), y=i)$minimum
  #cat(pretruncated.mean[i],", ", sep="")
  #cat(pretruncated.mean[i]/(1-exp(-pretruncated.mean[i])),", ",sep="")
  #cat(mean(rtpois.example(1000, pretruncated.mean[i])), fill=TRUE)
#}
#
#truncated.poisson.table <- data.frame(mean = 1:20, lambda = pretruncated.mean)
#rm(pretruncated.mean, find.pretruncated.mean, rtpois.example)



