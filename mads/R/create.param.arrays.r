create.param.arrays <- function(ddf.models, n, criteria){
  bootstrap.ddf.statistics <- list()
  for(sp in seq(along = ddf.models)){
    bootstrap.ddf.statistics[[names(ddf.models)[sp]]]$convergence <- array(0, dim = c(3,length(ddf.models[[sp]])), dimnames = list(c("Converged", "Failed to Converge", "Selected"), ddf.models[[sp]]))
    
    for(m in seq(along = ddf.models[[sp]])){
      model.name <- ddf.models[[sp]][m]
      model.type <- get(model.name)$method
      #create ds param array if required
      if(model.type%in%c("ds")){
        param.names <- NULL
        param.names <- colnames(get(model.name)$ds$aux$ddfobj$scale$dm) 
        param.names <- c(param.names, colnames(get(model.name)$ds$aux$ddfobj$shape$dm))
        adj.names <- NULL
        for(i in seq(along = get(model.name)$ds$aux$ddfobj$adjustment$parameters)){
          adj.names[i] <- paste(get(model.name)$ds$aux$ddfobj$adjustment$series, ", order ",get(model.name)$ds$aux$ddfobj$adjustment$order[i],sep="")
        }
        param.names <- c(param.names, adj.names)
        #param.names <- names(get(model.name)$ds$aux$ddfob$scale$parameters)
        #if(is.null(param.names)){
        #  param.names <- rep("scale",length(get(model.name)$ds$aux$ddfob$scale$parameters))
        #}
        #temp.names  <- names(get(model.name)$ds$aux$ddfob$shape$parameters)
        #if(!is.null(temp.names)){
        #  param.names <- c(param.names, temp.names)
        #}else{
        #  param.names <- c(param.names, rep("shape",length(get(model.name)$ds$aux$ddfob$shape$parameters)))
        #}
        #temp.names  <- names(get(model.name)$ds$aux$ddfob$adjustment$parameters) 
        #if(!is.null(temp.names)){
        #  param.names <- c(param.names, temp.names)
        #}else{
        #  param.names <- c(param.names, rep("adj",length(get(model.name)$ds$aux$ddfob$adjustment$parameters)))
        #}
        #CAN CHANGE ONCE ALL PARAMETERS ARE NAMED IN THE DDF MODEL OBJECT
        #param.names <- names(get(model.name)$ds$aux$ddfob$scale$parameters)
        #param.names <- c(param.names, names(get(model.name)$ds$aux$ddfob$shape$parameters))
        #param.names <- c(param.names, names(get(model.name)$ds$aux$ddfob$adjustment$parameters))
        bootstrap.ddf.statistics[[names(ddf.models)[sp]]][[model.name]]$ds.param <- array(dim = c(n,length(param.names)), dimnames = list(1:n, param.names))
      }
      if(model.type%in%c("trial", "io")){
        param.names <- NULL
        param.names <- colnames(get(model.name)$ds$aux$ddfobj$scale$dm) 
        param.names <- c(param.names, colnames(get(model.name)$ds$aux$ddfobj$shape$dm))
        adj.names <- NULL
        for(i in seq(along = get(model.name)$ds$aux$ddfobj$adjustment$parameters)){
          adj.names[i] <- paste(get(model.name)$ds$aux$ddfobj$adjustment$series, ", order ",get(model.name)$ds$aux$ddfobj$adjustment$order[i],sep="")
        }
        param.names <- c(param.names, adj.names)
        #param.names <- NULL
        #param.names <- names(get(model.name)$ds$ds$aux$ddfob$scale$parameters)
        #if(is.null(param.names)){
        #  param.names <- rep("scale",length(get(model.name)$ds$ds$aux$ddfob$scale$parameters))
        #}
        #temp.names  <- names(get(model.name)$ds$ds$aux$ddfob$shape$parameters)
        #if(!is.null(temp.names)){
        #  param.names <- c(param.names, temp.names)
        #}else{
        #  param.names <- c(param.names, rep("shape",length(get(model.name)$ds$ds$aux$ddfob$shape$parameters)))
        #}
        #temp.names  <- names(get(model.name)$ds$ds$aux$ddfob$adjustment$parameters) 
        #if(!is.null(temp.names)){
        #  param.names <- c(param.names, temp.names)
        #}else{
        #  param.names <- c(param.names, rep("adj",length(get(model.name)$ds$ds$aux$ddfob$adjustment$parameters)))
        #}
        #CAN CHANGE ONCE ALL PARAMETERS ARE NAMED IN THE DDF MODEL OBJECT
        #param.names <- names(get(model.name)$ds$aux$ddfob$scale$parameters)
        #param.names <- c(param.names, names(get(model.name)$ds$aux$ddfob$shape$parameters))
        #param.names <- c(param.names, names(get(model.name)$ds$aux$ddfob$adjustment$parameters))
        bootstrap.ddf.statistics[[names(ddf.models)[sp]]][[model.name]]$ds.param <- array(dim = c(n,length(param.names)), dimnames = list(1:n, param.names))
      }
      #create mr param array if required
      if(model.type%in%c("trial.fi", "io.fi")){ 
        param.names <- NULL
        param.names <- names(get(model.name)$mr$coefficients)
        bootstrap.ddf.statistics[[names(ddf.models)[sp]]][[model.name]]$mr.param <- array(dim = c(n,length(param.names)), dimnames = list(1:n, param.names))  
      }
      if(model.type%in%c("trial", "io")){ 
        param.names <- NULL
        param.names <- names(get(model.name)$mr$mr$coefficients)
        bootstrap.ddf.statistics[[names(ddf.models)[sp]]][[model.name]]$mr.param <- array(dim = c(n,length(param.names)), dimnames = list(1:n, param.names))  
      }
      #Same for all models
      bootstrap.ddf.statistics[[names(ddf.models)[sp]]][[model.name]][[criteria]] <- rep(NA,n)
      bootstrap.ddf.statistics[[names(ddf.models)[sp]]][[model.name]]$selected <- rep(0,n)
    }#next model
  }#next species code 
  return(bootstrap.ddf.statistics)  
}


