create.result.arrays <- function(species.name, species.code.definitions, region.table, clusters, n){

  identified.species <- NULL
  for(sp in seq(along = species.name)){
    if(length(species.code.definitions[[species.name[sp]]]) == 1){
      identified.species <- c(identified.species, species.name[sp]) 
    }
  }
  no.id.species   <- length(identified.species)                                       
  strata.name  <- as.character(region.table$Region.Label)
  no.strata    <- length(strata.name)
  if(no.strata == 1){
    strata.name <- NULL
    no.strata <- 0
  }
  #create arrays to record bootstrap results
  individual.summary <- array(dim=c(no.strata+1, 5, n, no.id.species), dimnames = list(c(strata.name, "Total"), c("Area", "CoveredArea", "Effort", "n", "ER"), 1:n, identified.species))                                    
  individual.N       <- array(dim=c(no.strata+1, 3, n, no.id.species), dimnames = list(c(strata.name, "Total"), c("Estimate", "df", "PercentUnidentified"), 1:n, identified.species))        
  if(!clusters){
    # NO CLUSTERS - store these arrays in a list
    bootstrap.results <- list(individual.summary = individual.summary, individual.N = individual.N) 
  }else{ 
    # CLUSTERS
    clusters.summary   <- array(dim=c(no.strata+1, 6, n, no.id.species), dimnames = list(c(strata.name, "Total"), c("Area", "CoveredArea", "Effort", "n", "k", "ER"), 1:n, identified.species))
    clusters.N         <- array(dim=c(no.strata+1, 3, n, no.id.species), dimnames = list(c(strata.name, "Total"), c("Estimate", "df", "PercentUnidentified"), 1:n, identified.species))
    Expected.S         <- array(dim=c(no.strata+1, 2, n, no.id.species), dimnames = list(c(strata.name, "Total"), c("Expected.S", "new.Expected.S"), 1:n, identified.species))
    # store these arrays in a list  
    bootstrap.results <- list(individual.summary = individual.summary, individual.N = individual.N, clusters.summary = clusters.summary, clusters.N = clusters.N, Expected.S = Expected.S)
  } 
  return(bootstrap.results)                               
}