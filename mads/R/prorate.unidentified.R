#' Prorate the estimated abundances of the unidentified sightings to the
#' other identified species categories.
#'
#' The prorating is done individually for each strata. It will prorate the 
#' unidentified abundance between the species as defined in the
#' species.code.definitions except where specified that a given species is not 
#' present in that strata as defined in the species.presence argument.   
#'  
#' @param dht.results a list of objects of class dht
#' @param species.code.definitions a list of character vectors detailing the 
#'   species codes associated with the unidentified code given as the element 
#'   name.
#' @param species.presence a list of character vectors defining the species
#'   present in each strata.
#' of identified species codes corresponding to which species are present in 
#' each strata.
#' @return a list of proprated results with an element for each species
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#'
prorate.unidentified <- function(dht.results, species.code.definitions, species.presence){
# prorate.unidentified function to rorate the estimated abundances of the unidentified sightings to the
# other identified species categories.
#
# Arguments:
#   dht.results              - a list of objects of class dht
#   species.code.definitions - a list of character vectors containing species names
#   species.presence         - a list of character vectors containing species names present in that strata
#
# Value:
#   a list of proprated results with an element for each species
#
# Function Calls: none
#
  #get all identified and unidentified codes
  identified.codes <- unidentified.codes <- NULL
  for(sp in seq(along = species.code.definitions)){
    if(length(species.code.definitions[[sp]]) == 1){
      identified.codes <- c(identified.codes, names(species.code.definitions)[sp])
    } else {
      unidentified.codes <- c(unidentified.codes, names(species.code.definitions)[sp])  
    }
  }
  
  #create results object to store results in
  results <- list()
  #add all data for identified codes to results
  for(id in seq(along = identified.codes)){
    results[[identified.codes[id]]]$individual$summary <- dht.results[[identified.codes[id]]]$individuals$summary[,1:6]
    results[[identified.codes[id]]]$individual$N <- dht.results[[identified.codes[id]]]$individuals$N[,1:2]
    results[[identified.codes[id]]]$clusters$summary <- dht.results[[identified.codes[id]]]$clusters$summary[,1:7]
    results[[identified.codes[id]]]$clusters$N <- dht.results[[identified.codes[id]]]$clusters$N[,1:2]
    results[[identified.codes[id]]]$Expected.S <- dht.results[[identified.codes[id]]]$Expected.S[,1:2]
  }
  
  #get names of strata
  strata <- names(species.presence)
  
  #for all unidentified codes
  for(unid in seq(along = unidentified.codes)){
    #get corresponding identified codes
    corresp.identifieds <- species.code.definitions[[unidentified.codes[unid]]]
    #for all strata
    for(st in seq(along = strata)){ 
      
      #check unidentifed category has abundance in this strata if not next
      unid.abundance <- dht.results[[unidentified.codes[unid]]]$individuals$N$Estimate[dht.results[[unidentified.codes[unid]]]$individuals$N$Label == strata[st]]
      cat("unid.abundance: ", unid.abundance, ", class(unid.abundance): ", class(unid.abundance), fill=T)
      if(length(unid.abundance) == 0){                                          #CHECK WHEN THIS IS EMPTY!!!
        next
      }else if(unid.abundance == 0){                
        next
      }
      unid.cluster.abundance <- dht.results[[unidentified.codes[unid]]]$clusters$N$Estimate[dht.results[[unidentified.codes[unid]]]$clusters$N$Label == strata[st]]
      species.present <- species.presence[[strata[st]]]
      #find species which are both contained in the unid category and present in this strata
      prorate.species <- corresp.identifieds[which(!is.na(match(corresp.identifieds, species.present)))] 
      prorate.species <- as.character(prorate.species)

      #get the abundances of each identified
      id.abundance <- id.cluster.abundance <- NULL
      for(psp in seq(along = prorate.species)){
        id.abundance[psp] <- dht.results[[prorate.species[psp]]]$individuals$N$Estimate[dht.results[[prorate.species[psp]]]$individuals$N$Label == strata[st]]
        id.cluster.abundance[psp] <- dht.results[[prorate.species[psp]]]$clusters$N$Estimate[dht.results[[prorate.species[psp]]]$clusters$N$Label == strata[st]]
      } 
      names(id.abundance) <- prorate.species
      names(id.cluster.abundance) <- prorate.species
      #prorate the unidentified 
      prorated.unid.abundance <- (id.abundance/sum(id.abundance))*unid.abundance
      prorated.unid.abundance.cluster <- (id.cluster.abundance/sum(id.cluster.abundance))*unid.cluster.abundance  
      for(psp in seq(along = prorate.species)){
        results[[prorate.species[psp]]]$individual$N$Estimate[results[[prorate.species[psp]]]$individual$N$Label == strata[st]] <- prorated.unid.abundance[[prorate.species[psp]]] + results[[prorate.species[psp]]]$individual$N$Estimate[results[[prorate.species[psp]]]$individual$N$Label == strata[st]]
        cat(prorated.unid.abundance[[prorate.species[psp]]]," was added to species ", prorate.species[psp]," in strata ", strata[st]," from unidentified code ", unidentified.codes[unid],". Original abundance: ",id.abundance[psp], sep="", fill=T)
        results[[prorate.species[psp]]]$clusters$N$Estimate[results[[prorate.species[psp]]]$clusters$N$Label == strata[st]] <- prorated.unid.abundance.cluster[[prorate.species[psp]]] + results[[prorate.species[psp]]]$clusters$N$Estimate[results[[prorate.species[psp]]]$clusters$N$Label == strata[st]]
        cat(prorated.unid.abundance.cluster[[prorate.species[psp]]]," clusters were added to species ", prorate.species[psp]," in strata ", strata[st]," from unidentified code ", unidentified.codes[unid],". Original abundance: ",id.cluster.abundance[psp], sep="", fill=T)
      }
    }#next strata
  }#next unidentified code   
  
  #update totals and include percentage from unidentifieds
  for(r in seq(along = results)){
    index <- which(results[[1]]$individual$N$Label == "Total")
    #update totals
    results[[identified.codes[r]]]$individual$N$Estimate[index] <- sum(results[[identified.codes[r]]]$individual$N$Estimate[1:(index-1)])
    results[[identified.codes[r]]]$clusters$N$Estimate[index] <- sum(results[[identified.codes[r]]]$clusters$N$Estimate[1:(index-1)])
    #add in information regarding what percentage came from unidentified sightings
    results[[identified.codes[r]]]$individual$N <- cbind(results[[identified.codes[r]]]$individual$N, PercentUnidentified = round(((results[[identified.codes[r]]]$individual$N$Estimate-dht.results[[identified.codes[r]]]$individuals$N$Estimate)/results[[identified.codes[r]]]$individual$N$Estimate)*100,4))
    results[[identified.codes[r]]]$clusters$N <- cbind(results[[identified.codes[r]]]$clusters$N, PercentUnidentified = round(((results[[identified.codes[r]]]$clusters$N$Estimate-dht.results[[identified.codes[r]]]$clusters$N$Estimate)/results[[identified.codes[r]]]$clusters$N$Estimate)*100,4))
    #Calculate new expected cluster size
    results[[identified.codes[r]]]$Expected.S$new.Expected.S <- results[[identified.codes[r]]]$individual$N$Estimate/results[[identified.codes[r]]]$cluster$N$Estimate 
  } 
  #return results        
  return(results)
}



