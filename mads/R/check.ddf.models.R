#' Checks the list of model names supplied by the user
#'
#' Performs various checks on the model names supplied by the user. If the
#' multi-analysis engine is being called from Distance then the model names are
#' transformed from numbers to names. This also checks that the data in each
#' model are the same across species codes to ensure valid comparison using the
#' AIC/AICc/BIC selection criteria.
#'  
#' @param ddf.models a list of vectors of model names. Each list element is
#'   named according to the species code.
#' @param species.name a character vector of species codes
#' @param dist.names boolean TRUE if only numbers are specified and the Distance
#'   naming convention is being used. FALSE if complete character model names
#'   are supplied.
#' @return updated list of vectors of model names if only numbers were supplied
#'   otherwise ddf.models is returned unchanged. 
#' @note Internal function not intended to be called by user.
#' @author Laura Marshall
#' @seealso \code{execute.multi.analysis}
#' @keywords input validation, data validation
#'

check.ddf.models <- function(ddf.models, species.name, dist.names){             
# 
# check.ddf.models - Performs various checks on the model names supplied by the user
#
# Arguments:
#
#  ddf.models   - a list of vectors specifying the model names for each species code
#  species.name - a vector of species names 
#  dist.names   -   
#
# Value: updated list of vectors of model names if only numbers were supplied
#   otherwise ddf.models is returned unchanged.
# 
# Function calls: 
#   is.same - local function to compare two datasets to see if they match
#                                      

#add a check that all the detected are 1 in the ddf data [used to be a problem in mrds]
  
  is.same <- function(data1, data2){
  # is.same function to compare two datasets to see if they match
  #
  # Arguments:
  #   data1 - dataframe for comparison
  #   data2 - dataframe for comparison
  #
  # Value: returns numeric
  #   0 - dataframes are the same
  #   1 - differ in number of observations
  #   2 - differ in object ID and/or distances
  #  
    #if the dataframes have different lengths return 1
    if(nrow(data1) != nrow(data2)){
      return(1)
    }
    #If they are of the same dimension check if they are identical
    if(ncol(data1) == ncol(data2)){
      compare <- data1 == data2
      #if the dataframes are identical return 0
      if(length(which(compare == FALSE)) == 0){
        return(0)
      }
    }
    #order data by object and compare object id's and distances
    compare.data1 <- data1[order(data1$object), c("object","distance")]
    compare.data2 <- data2[order(data2$object), c("object","distance")]
    compare <- compare.data1 == compare.data2
    if(length(which(compare == FALSE)) == 0){
      return(0)
    }else {
      return(2)
    }
  }
  
  #RENAME MODELS IF DISTANCE NAMING CONVENTION WAS USED AND ONLY NUMBERS WERE PROVIDED
  if(dist.names){
    for(sp in seq(along = ddf.models)){     
      #for every model
      for(m in seq(along = ddf.models[[sp]])){ 
        ddf.models[[sp]][m] <- paste("ddf.",ddf.models[[sp]][m],sep="")
      }#next model
    }# next species
  }
  #CHECK DATA MATCHES ACROSS DIFFERENT MODELS FOR THE SAME SPECIES
  for(sp in seq(along = ddf.models)){     
    for(m in seq(along = ddf.models[[sp]])){ 
      #check model exists 
      ddf.data <- try(get(ddf.models[[sp]][m])$data, silent = TRUE)
      if(class(ddf.data)[1] == "try-error"){
        #ddf object doesn't exist
        stop(paste("ddf object ",m," (analysis name ",ddf.models[[sp]][m],") for species code ",species.name[sp]," does not exist.",sep = ""))
      }else if(m == 1){
        #Get first dataset to compare all others too
        check.data <- ddf.data
      }else {
        #Compare subsequent datasets to first dataset     
        if(is.same(check.data, ddf.data) != 0){
          stop(paste("Datasets within species must contain the same data to ensure the model selection criteria are valid. The ",species.name[sp]," analyses ",ddf.models[[sp]][1]," and ",ddf.models[[sp]][m]," do not have the same sightings and/or associated distances", sep = ""))
        }
      }
    }#next model
  }#next species
  return(ddf.models)
}