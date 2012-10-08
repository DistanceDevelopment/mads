check.bootstrap.options <- function(bootstrap, resample, n, sample.table){
  if(bootstrap & resample == "samples"){
    no.samples <- table(sample.table$Region.Label)
    insufficient.samples <- no.samples[no.samples < 20]
    for(i in seq(along = insufficient.samples)){
      message(paste("Warning: Only ",insufficient.samples[i]," transect(s) in strata ",names(insufficient.samples)[i],". The estimates of variance are likely to be biased.", sep = ""))
    }
  }
  if(!bootstrap){
    cat("Warning: Currently the only method of variance estimation in mads is via the bootstrap.", sep = "", fill=TRUE)
  }  
}