create.param.arrays <- function(model.names, ddf.models, n, criteria){
  bootstrap.ddf.statistics <- list()
  for(sp in seq(along = model.names)){
    bootstrap.ddf.statistics[[names(model.names)[sp]]]$convergence <- array(0, dim = c(3,length(model.names[[sp]])), dimnames = list(c("Converged", "Failed to Converge", "Selected"), model.names[[sp]]))           
    for(m in seq(along = model.names[[sp]])){
      current.model.name <- model.names[[sp]][m]
      current.model <- ddf.models[[current.model.name]]
      model.type <- current.model$method
      #create ds param array if required
      if(model.type%in%c("ds")){
        param.names <- NULL
        param.names <- colnames(current.model$ds$aux$ddfobj$scale$dm) 
        param.names <- c(param.names, colnames(current.model$ds$aux$ddfobj$shape$dm))
        adj.names <- NULL
        for(i in seq(along = current.model$ds$aux$ddfobj$adjustment$parameters)){
          adj.names[i] <- paste(current.model$ds$aux$ddfobj$adjustment$series, ", order ",current.model$ds$aux$ddfobj$adjustment$order[i],sep="")
        }
        param.names <- c(param.names, adj.names)
        #param.names <- names(current.model$ds$aux$ddfob$scale$parameters)
        #if(is.null(param.names)){
        #  param.names <- rep("scale",length(current.model$ds$aux$ddfob$scale$parameters))
        #}
        #temp.names  <- names(current.model$ds$aux$ddfob$shape$parameters)
        #if(!is.null(temp.names)){
        #  param.names <- c(param.names, temp.names)
        #}else{
        #  param.names <- c(param.names, rep("shape",length(current.model$ds$aux$ddfob$shape$parameters)))
        #}
        #temp.names  <- names(current.model$ds$aux$ddfob$adjustment$parameters) 
        #if(!is.null(temp.names)){
        #  param.names <- c(param.names, temp.names)
        #}else{
        #  param.names <- c(param.names, rep("adj",length(current.model$ds$aux$ddfob$adjustment$parameters)))
        #}
        #CAN CHANGE ONCE ALL PARAMETERS ARE NAMED IN THE DDF MODEL OBJECT
        #param.names <- names(current.model$ds$aux$ddfob$scale$parameters)
        #param.names <- c(param.names, names(current.model$ds$aux$ddfob$shape$parameters))
        #param.names <- c(param.names, names(current.model$ds$aux$ddfob$adjustment$parameters))
        bootstrap.ddf.statistics[[names(model.names)[sp]]][[current.model.name]]$ds.param <- array(dim = c(n,length(param.names)), dimnames = list(1:n, param.names))
      }
      if(model.type%in%c("trial", "io")){
        param.names <- NULL
        param.names <- colnames(current.model$ds$aux$ddfobj$scale$dm) 
        param.names <- c(param.names, colnames(current.model$ds$aux$ddfobj$shape$dm))
        adj.names <- NULL
        for(i in seq(along = current.model$ds$aux$ddfobj$adjustment$parameters)){
          adj.names[i] <- paste(current.model$ds$aux$ddfobj$adjustment$series, ", order ",current.model$ds$aux$ddfobj$adjustment$order[i],sep="")
        }
        param.names <- c(param.names, adj.names)
        #param.names <- NULL
        #param.names <- names(current.model$ds$ds$aux$ddfob$scale$parameters)
        #if(is.null(param.names)){
        #  param.names <- rep("scale",length(current.model$ds$ds$aux$ddfob$scale$parameters))
        #}
        #temp.names  <- names(current.model$ds$ds$aux$ddfob$shape$parameters)
        #if(!is.null(temp.names)){
        #  param.names <- c(param.names, temp.names)
        #}else{
        #  param.names <- c(param.names, rep("shape",length(current.model$ds$ds$aux$ddfob$shape$parameters)))
        #}
        #temp.names  <- names(current.model$ds$ds$aux$ddfob$adjustment$parameters) 
        #if(!is.null(temp.names)){
        #  param.names <- c(param.names, temp.names)
        #}else{
        #  param.names <- c(param.names, rep("adj",length(current.model$ds$ds$aux$ddfob$adjustment$parameters)))
        #}
        #CAN CHANGE ONCE ALL PARAMETERS ARE NAMED IN THE DDF MODEL OBJECT
        #param.names <- names(current.model$ds$aux$ddfob$scale$parameters)
        #param.names <- c(param.names, names(current.model$ds$aux$ddfob$shape$parameters))
        #param.names <- c(param.names, names(current.model$ds$aux$ddfob$adjustment$parameters))
        bootstrap.ddf.statistics[[names(model.names)[sp]]][[current.model.name]]$ds.param <- array(dim = c(n,length(param.names)), dimnames = list(1:n, param.names))
      }
      #create mr param array if required
      if(model.type%in%c("trial.fi", "io.fi")){ 
        param.names <- NULL
        param.names <- names(current.model$mr$coefficients)
        bootstrap.ddf.statistics[[names(model.names)[sp]]][[current.model.name]]$mr.param <- array(dim = c(n,length(param.names)), dimnames = list(1:n, param.names))  
      }
      if(model.type%in%c("trial", "io")){ 
        param.names <- NULL
        param.names <- names(current.model$mr$mr$coefficients)
        bootstrap.ddf.statistics[[names(model.names)[sp]]][[current.model.name]]$mr.param <- array(dim = c(n,length(param.names)), dimnames = list(1:n, param.names))  
      }
      #Same for all models
      bootstrap.ddf.statistics[[names(model.names)[sp]]][[current.model.name]][[criteria]] <- rep(NA,n)
      bootstrap.ddf.statistics[[names(model.names)[sp]]][[current.model.name]]$selected <- rep(0,n)
      bootstrap.ddf.statistics[[names(model.names)[sp]]][[current.model.name]]$model.description <- model.description(current.model)
    }#next model
  }#next species code 
  return(bootstrap.ddf.statistics)  
}


