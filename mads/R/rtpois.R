rtpois <-
function(N, mean=NA, lambda=NA){
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
