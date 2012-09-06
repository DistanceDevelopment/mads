store.param.ests <- function(bootstrap.ddf.statistics, species.name, model.name, ddf.model, rep.no){
  model.type <- ddf.model$method
  if(model.type%in%c("ds")){
    param.ests <- c(ddf.model$ds$aux$ddfob$scale$parameters, ddf.model$ds$aux$ddfob$shape$parameters, ddf.model$ds$aux$ddfob$adjustment$parameters)
    bootstrap.ddf.statistics[[species.name]][[model.name]]$ds.param[rep.no, 1:length(param.ests)] <- param.ests
  }
  if(model.type%in%c("trial", "io")){
    param.ests <- c(ddf.model$ds$ds$aux$ddfob$scale$parameters, ddf.model$ds$ds$aux$ddfob$shape$parameters, ddf.model$ds$ds$aux$ddfob$adjustment$parameters)
    bootstrap.ddf.statistics[[species.name]][[model.name]]$ds.param[rep.no, 1:length(param.ests)] <- param.ests
  }
  if(model.type%in%c("trial", "io")){
    param.ests <- ddf.model$mr$mr$coefficients
    bootstrap.ddf.statistics[[species.name]][[model.name]]$mr.param[rep.no, 1:length(param.ests)] <- param.ests
  }
  if(model.type%in%c("trial.fi", "io.fi")){
    param.ests <- ddf.model$mr$coefficients
    bootstrap.ddf.statistics[[species.name]][[model.name]]$mr.param[rep.no, 1:length(param.ests)] <- param.ests
  }
  return(bootstrap.ddf.statistics)
}


