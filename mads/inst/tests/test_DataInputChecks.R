library(mads)
library(mrds)
library(testthat)

answer<-readline("\nWarning: running test_package creates global objects, 
                  \nit is therefore advised to only run this in a clean 
                  \nR workspace. Do you wish to continue? (y/n)? ")
answer<-as.character(answer)
if(answer == "y" | answer == "Y"){


  context("Data input")

  test_that("Test data input checks", {


    #datasetup
    ex.filename<-system.file("testData/input_checks/ddf_dat.robj", package="mads")
    load(ex.filename)
    ex.filename<-system.file("testData/input_checks/obs_table.robj", package="mads")
    load(ex.filename)
    ex.filename<-system.file("testData/input_checks/region_table.robj", package="mads")
    load(ex.filename)
    ex.filename<-system.file("testData/input_checks/sample_table.robj", package="mads")
    load(ex.filename)
    
    #run ddf analyses
    ddf.1 <<- ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='ds', data=ddf.dat,meta.data=list(width=4)) 
    ddf.2 <<- ddf(dsmodel = ~mcds(key = "hr", formula = ~ size), method='ds', data=ddf.dat,meta.data=list(width=4)) 
    ddf.3 <<- ddf(dsmodel = ~mcds(key = "unif", formula = ~ 1, adj.series = "cos", adj.order = c(2)), method='ds', data=ddf.dat,meta.data=list(width=4))
    ddf.4 <<- ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='trial',mrmodel=~glm(link='logit',formula=~distance + size + sex + exposure), data=ddf.dat,meta.data=list(width=4)) 
    ddf.5 <<- ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='trial.fi',mrmodel=~glm(link='logit',formula=~distance + size + sex + exposure), data=ddf.dat,meta.data=list(width=4)) 
    ddf.6 <<- ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='io',mrmodel=~glm(link='logit',formula=~distance + size + sex + exposure), data=ddf.dat,meta.data=list(width=4)) 
    ddf.7 <<- ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='io.fi',mrmodel=~glm(link='logit',formula=~distance + size + sex + exposure), data=ddf.dat,meta.data=list(width=4))
    ddf.8 <<- ddf.7
    class(ddf.8) <<- c("rem", "ddf") 
    ddf.9 <<- ddf.7
    class(ddf.8) <<- c("glm")
    ddf.10 <<- ddf.3
    ddf.10$data  <<- ddf.10$data[,-3]
    
    #set up information for mads
    #Multi-analysis options
    ddf.models               <- list("CD"=c(1,2), "WD"=c(1,2), "UnidDol"=c(1,2))
    species.code.definitions <- list("UnidDol"=c("CD","WD"))      
    species.presence         <- list("1. A" = c("CD","WD"))                 
    covariate.uncertainty    <- NULL
    ddf.model.options        <- list(distance.naming.conv=TRUE, criterion="AIC", max.refit = 0)
    ddf.model.options$distance.naming.conv <- TRUE                                
    bootstrap                <- TRUE                                              
    bootstrap.options        <- list(resample="samples", n=5)                     
    seed.array               <- NULL   
    
    #~~~~~~~~~~~~~~~~~~~~~~~~ TEST check.ddf.models(...) ~~~~~~~~~~~~~~~~~~~~~~~~~
  
    # check that there is an error when the same model is selected multiple times for a given species
    ddf.models               <- list("CD"=c(1,1), "WD"=c(1,2), "UnidDol"=c(1,2))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : The model names are not unique for species CD."))
    # check that there is an error when ds models are mixed with io/trial
    ddf.models               <- list("CD"=c(1,4), "WD"=c(1,2), "UnidDol"=c(1,2))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error())#"Error : Models must either be all mark-recapture (double observer) or all standard distance sampling models, not a mixture."))
    # check that there is an error when io models are mixed with trial
    ddf.models               <- list("CD"=c(4,5), "WD"=c(4,5), "UnidDol"=c(4,7))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Models must either be all trial or all io, not a mixture."))
    # check that there is an error when models don't exists
    ddf.models               <- list("CD"=c(100,5), "WD"=c(4,5), "UnidDol"=c(4,5))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error())#"Error : ddf object 1 (analysis name ddf.100) for species code CD does not exist."))
    # check that there is an error when some models contain data with cluster size and some contain data withough cluster size
    ddf.models               <- list("CD"=c(2,3), "WD"=c(2,3), "UnidDol"=c(2,10))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Cluster size must be present in all datasets within the ddf models or none."))  
             
    #~~~~~~~~~~~~~~~~ TEST check.species.code.definitions(...) ~~~~~~~~~~~~~~~~~~~
    
    ddf.models               <- list("CD"=c(1,2), "WD"=c(1,2), "UnidDol"=c(1,2))
    species.code.definitions <- list("UnidDol"=c("CD","WD"), "CD"=NULL)
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : No species codes specified for CD in the species code definitions list."))
    species.code.definitions <- list("UnidDol"=c("CD","WD"), "CP"="CP")
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Species mismatch in ddf models and species code definitions. Models not suppled for all species or models supplied for species not included in species code definitions."))
    species.code.definitions <- list("UnidDol"=c("CD","WD"), "CD"="WD")
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Incorrect species code definition for species CD. If only a single code is entered it must match the name of the list element."))
    species.code.definitions <- list("UnidDol"=c("UnidDol","CD","WD"))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Incorrect species code definition for species UnidDol. Unidentified code cannot be prorated to itself."))
    species.code.definitions <- list("UnidDol"=c("CD","WD"), "UnidDel" = c("CD", "WD", "UnidDol"))
    ddf.models               <- list("CD"=c(1,2), "WD"=c(1,2), "UnidDol"=c(1,2), "UnidDel"=c(1,2))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Incorrect species code definition for species UnidDel. An Unidentified code cannot be prorated to another unidentified code."))
    species.code.definitions <- list("UnidDol"=c("CD","WD"), "UnidDol"=c("CD","HP"))
    ddf.models               <- list("CD"=c(1,2), "WD"=c(1,2), "UnidDol"=c(1,2))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Multiple species code entries in the species code definitions list."))
    species.code.definitions <- list("UnidDol"=c("CD","WD"))
    ddf.models               <- list("CD"=c(1,2), "WD"=c(1,2), "UnidDol"=c(1,2), "HP"=c(1,2))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Species mismatch in ddf models and species code definitions. Models not suppled for all species or models supplied for species not included in species code definitions."))
    
    #~~~~~~~~~~~~~~~~~~~ TEST check.covar.uncertainty(...) ~~~~~~~~~~~~~~~~~~~~~~~
    
    ddf.models               <- list("CD"=c(1,2), "WD"=c(1,2), "UnidDol"=c(1,2))
    covariate.uncertainty = data.frame(variable.layer = c("observation"), variable.name = c("scaledtotsize"), variable.correction.factor = c(1), uncertainty.layer = c("observation"), uncertainty.name = c("totsizecv"), uncertainty.measure = c("CV"), sampling.distribution = c("Norm"))
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : An unsupported sampling distribution has been chosen for covariate uncertainty. Only one of the following may be specified: Normal, Normal.Absolute, Lognormal.BC, Poisson, TruncPoisson.BC"))
    covariate.uncertainty = data.frame(variable.layer = c("observation"), variable.name = c("scaledtotsize"), variable.correction.factor = c(1), uncertainty.layer = c("observation"), uncertainty.name = c("totsizecv"), uncertainty.measure = c("CV"), sampling.distribution = c("Normal"))
    
    expect_that(results <- execute.multi.analysis(region.table=region.table, 
                 sample.table=sample.table, obs.table=obs.table, bootstrap, 
                 bootstrap.options, covariate.uncertainty=covariate.uncertainty, 
                 ddf.models, ddf.model.options = ddf.model.options, 
                 species.code.definitions, species.presence),
             throws_error("Error : Invalid names for the covariates or associated uncertainty have been specified in the covariate uncertainty dataframe.")) 
  })
}