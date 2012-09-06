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
  
  clusters <- FALSE
  double.observer <- NULL
  
  #RENAME MODELS IF DISTANCE NAMING CONVENTION WAS USED AND ONLY NUMBERS WERE PROVIDED
  if(dist.names){
    for(sp in seq(along = ddf.models)){     
      #for every model
      for(m in seq(along = ddf.models[[sp]])){ 
        ddf.models[[sp]][m] <- paste("ddf.",ddf.models[[sp]][m],sep="")
      }#next model
    }# next species
  }
  #CHECK WHETHER IT IS A MR DOUBLE OBSERVER ANALYSIS
  model.type <- NULL
  counter <- 1
  for(sp in seq(along = ddf.models)){     
    #for every model
    for(m in seq(along = ddf.models[[sp]])){
      model.type[counter] <- get(ddf.models[[sp]][m])$method  
      counter <- counter + 1
    }#next model
  }# next species
  double.observer <- which(model.type%in%c("trial", "trial.fi", "io", "io.fi"))
  ds <- which(model.type%in%c("ds"))
  unsupported <- which(!model.type%in%c("trial", "trial.fi", "io", "io.fi", "ds"))
  if(length(double.observer) == length(model.type)){
    double.observer <- TRUE
    #check all are trial or all are io
    if(!(length(which(model.type%in%c("trial", "trial.fi"))) == length(model.type) | length(which(model.type%in%c("io", "io.fi"))) == length(model.type))){
      process.warnings()
      stop(paste("Models must either be all trial or all io, not a mixture.", sep = ""), call. = FALSE)
    }
  }else if(length(ds) == length(model.type)){
    double.observer <- FALSE
  }else if(length(double.observer) > 0 & length(ds) > 0){
    process.warnings()
    stop("Models must either be all mark-recapture (double observer) or all standard distance sampling models, not a mixture.", call. = FALSE)
  }else{
    process.warnings()
    stop(paste("Unsupported model types have been selected: ",paste(model.type[unsupported], collapse = ", "), sep = ""), call. = FALSE)
  } 
  rm(model.type, counter, ds, unsupported)

  #CHECK THAT MODELS FOR EACH SPECIES ARE UNIQUE
  for(sp in seq(along = ddf.models)){
    model.names <- ddf.models[[sp]]
    for(m in seq(along = model.names)){
      for(mcheck in seq(along = model.names)){
        if(m == mcheck){
          next
        }else if(model.names[m] == model.names[mcheck]){
          process.warnings()
          stop(paste("The model names are not unique for species ",names(ddf.models)[sp],".", sep = ""), call. = FALSE)
        }
      }
    }   
  }
  #CHECK DATA MATCHES ACROSS DIFFERENT MODELS FOR THE SAME SPECIES
  for(sp in seq(along = ddf.models)){     
    for(m in seq(along = ddf.models[[sp]])){ 
      #check model exists 
      ddf.data <- try(get(ddf.models[[sp]][m])$data, silent = TRUE)
      if(class(ddf.data)[1] == "try-error"){
        #ddf object doesn't exist
        process.warnings()
        stop(paste("ddf object ",m," (analysis name ",ddf.models[[sp]][m],") for species code ",species.name[sp]," does not exist.",sep = ""), call. = FALSE)
      }else if(m == 1){
        #Get first dataset to compare all others too
        check.data <- ddf.data
      }else if(is.same(check.data, ddf.data) != 0){
        process.warnings()
        stop(paste("Datasets within species must contain the same data to ensure the model selection criteria are valid. The ",species.name[sp]," analyses ",ddf.models[[sp]][1]," and ",ddf.models[[sp]][m]," do not have the same sightings and/or associated distances", sep = ""), call. = FALSE)
      }
    }#next model
    #CHECK IF DATA CONTAINS CLUSTER SIZES "size" (either all must or all most not)
    if("size"%in%names(ddf.data)){
      if(sp > 1 & !clusters){
        process.warnings()
        stop("Cluster size must be present in all datasets within the ddf models or none.", call. = FALSE)
      }
      clusters <- TRUE
    }else if(clusters){
      process.warnings()
      stop("Cluster size must be present in all datasets within the ddf models or none.", call. = FALSE)
    }    
  }#next species
  return(list(ddf.models = ddf.models, clusters = clusters, double.observer = double.observer))
}