library(mads)
library(mrds)
library(testthat)

context("Data Input")

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
  
  #run analyses
  ddf.1<-ddf(dsmodel = ~mcds(key = "hr", formula = ~ size), method='ds', data=ddf.dat,meta.data=list(width=4)) 
  ddf.2<-ddf(dsmodel = ~mcds(key = "hr", formula = ~ size), method='ds', data=ddf.dat,meta.data=list(width=4)) 
  ddf.3<-ddf(dsmodel = ~mcds(key = "unif", formula = ~ 1, adj.series = "poly", adj.order = c(2,4)), method='ds', data=ddf.dat,meta.data=list(width=4))
  ddf.4<-ddf(dsmodel = ~mcds(key = "unif", formula = ~ 1, adj.series = "poly", adj.order = c(2,4)), method='ds', data=ddf.dat,meta.data=list(width=4))
  ddf.5<-ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='trial',mrmodel=~glm(link='logit',formula=~distance + size + sex + exposure), data=ddf.dat,meta.data=list(width=4)) 
  ddf.6<-ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='trial',mrmodel=~glm(link='logit',formula=~distance + size + sex + exposure), data=ddf.dat,meta.data=list(width=4)) 
  ddf.7<-ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='trial',mrmodel=~glm(link='logit',formula=~distance + size + sex + exposure), data=ddf.dat,meta.data=list(width=4)) 
  ddf.8<-ddf(dsmodel = ~mcds(key = "hn", formula = ~ size), method='trial',mrmodel=~glm(link='logit',formula=~distance + size + sex + exposure), data=ddf.dat,meta.data=list(width=4))
  ddf.9 <- ddf.8
  class(ddf.9) <- c("rem", "ddf") 

  # check that there is an error
   expect_that(result<-ddf(dsmodel = ~mcds(key = "hn", formula = ~sex), 
                   data = egdata[egdata$observer ==1, ], method = "ds", 
                   meta.data = list(width = 4,mono=TRUE)),
               throws_error("Covariate models cannot be constrained for monotonicity."))
  })
