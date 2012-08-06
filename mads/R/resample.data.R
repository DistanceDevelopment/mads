#' Resamples the data for the bootstrap
#'
#' Can either resample samples within strata or observations within samples
#'  
#' @param resample if "samples" resample samples within strata, if 
#'   "observations" resample observations within samples.
#' @param obs.table dataframe of observation records with fields object,
#'   Region.Label, and Sample.Label which give links to sample.table,
#'   region.table and the data records used in \code{model}
#' @param sample.table dataframe of sample records - Region.Label,
#'   Sample.Label, Effort
#' @param ddf.dat.master list of complete/original datasets
#' @return a list with 2 elements: 
#'   ddf.dat.working a list of resampled datasets to be used in the analyses 
#'   obs.table an updated obs.table with additional entries for data replicates
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#' @keywords data manipulation
#'
resample.data <- function(resample, obs.table, sample.table, ddf.dat.master){
# resample.data function to non-parametrically resample the observations
#
# Arguments:
#   resample       - character option either "samples" or "observations"
#   obs.table      - dataframe of observation records
#   sample.table   - dataframe of sample records
#   ddf.dat.master - list of original datasets
#
# Value: list containind the following elements
#   ssf.dat.working - a list of resampled datasets
#   obs.table       - an updated obs.table
#
# Functions Used: renumber.duplicates 
#
  #create data storage
  ddf.dat.working <- list()
  species.name <- names(ddf.dat.master)

  #RESAMPLE SAMPLES WITHIN STRATA
  if(resample == "samples"){
    #Test to make sure that region labels are unique
    #if(length(unique(region.dat$Region.Label))!=length(region.dat$Region.Label))
    #  stop("Region labels must be unique")
    #samples=merge(region.dat,sample.dat,by.x="Region.Label",all.x=TRUE,all.y=TRUE)
    region.names <- as.character(unique(sample.table$Region.Label))
    
    #for every strata
    for(r in seq(along = region.names)){
      #get sample IDs
      temp <- sample.table[sample.table$Region.Label == region.names[r],]
      #Test to see if all sample labels are unique within strata?
      #resample
      new.samples <- data.frame(Sample.Label = sample(temp$Sample.Label, nrow(temp), replace = TRUE))
      #new.samples <- data.frame(Sample.Label = sample(temp$Sample.Label, 1, replace = TRUE))
      #get observation numbers
      if(r == 1){
        resample.result <- merge(new.samples, obs.table, by = "Sample.Label")
        resample.result <- resample.result[order(resample.result$object),]
      }else{
        resample.result <- rbind(resample.result, merge(new.samples, obs.table, by = "Sample.Label"))
      }
    }
    #get data for chosen observations 
    for(sp in seq(along = species.name)){
      ddf.dat.working[[species.name[sp]]] <- merge(ddf.dat.master[[species.name[sp]]], resample.result, by='object')[,1:ncol(ddf.dat.master[[species.name[sp]]])]
      renumber.duplicate.results <- renumber.duplicates(ddf.dat.working[[species.name[sp]]], obs.table)
      ddf.dat.working[[species.name[sp]]] <- renumber.duplicate.results[[1]]
      obs.table <- renumber.duplicate.results[[2]]
      #ddf.dat.working[[species.name[sp]]]$old.object.id <- ddf.dat.working[[species.name[sp]]]$object
      #ddf.dat.working[[species.name[sp]]]$object <- 1:nrow(ddf.dat.working[[species.name[sp]]])
    }
  #RESAMPLE OBSERVATIONS WITHING SAMPLES   
  }else if(resample == "observations"){
    #for every species
    for(sp in seq(along = species.name)){
      #get sample ID's for data
      temp.dat <- merge(ddf.dat.master[[species.name[sp]]], obs.table, by = 'object')
      sample.id <- unique(temp.dat$Sample.Label)
      #for every sample
      for(samp in seq(along = sample.id)){
        #get observation IDs
        object.id <- temp.dat$object[which(temp.dat$Sample.Label == sample.id[samp])]
        #resample
        if(length(object.id) > 1){
          new.obs <- data.frame(object = sample(object.id, length(object.id), replace = TRUE))
        }else{  #sample() does something funny if it is only given one value
          new.obs <- data.frame(object = object.id)
        }
        #find observations
        if(samp == 1){
          resample.result <- merge(new.obs, ddf.dat.master[[species.name[sp]]], by = 'object')
        }else{    
          resample.result <- rbind(resample.result, merge(new.obs, ddf.dat.master[[species.name[sp]]], by = 'object'))
        }         
      }#next sample
      ddf.dat.working[[species.name[sp]]] <- resample.result
      renumber.duplicate.results <- renumber.duplicates(ddf.dat.working[[species.name[sp]]], obs.table)
      ddf.dat.working[[species.name[sp]]] <- renumber.duplicate.results[[1]]
      obs.table <- renumber.duplicate.results[[2]]
      #ddf.dat.working[[species.name[sp]]]$old.object.id <- ddf.dat.working[[species.name[sp]]]$object
      #ddf.dat.working[[species.name[sp]]]$object <- 1:nrow(ddf.dat.working[[species.name[sp]]])
    }#next species  
  }#end elseif 
  
  return(list(ddf.dat.working, obs.table))
}