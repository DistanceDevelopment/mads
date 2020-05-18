#' Multi-Analysis Distance Sampling (mads)
#'
#' Can perform distance sampling analyses on a number of species at once
#'   and can account for unidentified sightings. Unidentified sightings 
#'   refer to sightings which cannot be allocated to a single species but 
#'   may instead be allocated to a group of species. The abundance of each
#'   unidentified group is estimated and then prorated to the species
#'   estimates. The multi-analysis engine can also incorporate model and
#'   covariate uncertainty. Variance estimation is via a non parametric
#'   bootstrap. The methods implemented are described in Gerodette T. and 
#'   Forcada J. (2005) <10.3354/meps291001> "Non-recovery of two spotted 
#'   and spinner dolphin populations in the eastern tropical Pacific Ocean".
#'
#'  The main function in this package is \link{execute.multi.analysis}.
#'
#' Further information on distance sampling methods and example code is available at \url{http://distancesampling.org/R/}.
#'
#' We are also in the process of setting up a new area of the website for vignettes / example code at \url{http://examples.distancesampling.org }. 
#'
#' For help with distance sampling and this package, there is a Google Group \url{https://groups.google.com/forum/#!forum/distance-sampling}.
#'
#' @name mads-package
#' @aliases mads-package mads
#' @docType package
#' @author Laura Marshall <lhm@@st-and.ac.uk>
#' @keywords package
#'
NULL

#' Example simulated data used to demonstrate the package functionality
#'
#' These data were generated using DSsim. Two populations were generated inside
#' a rectangular study region, one of these is called 'CD' (common dolphin) and the 
#' other is 'WSD' (white-sided dolphin). Density was assumed to be equal across the 
#' study area and the population sizes for the CD and WSD populations were 3000
#' and 1500, respectively. Detections of individuals were simulated based on half
#' normal detection functions with a scale parameter of 0.5 and a truncation 
#' distance of 1. A systematic parallel line transect design was used. Once both 
#' sets of data had been generated they were combined and 10% of the sightings were
#' randomly selected to be in the unidentified sightings category.
#'
#' @name mads.data
#' @docType data
#' @format This is a list of 4 items. The first is dist.data a dataframe with 
#' distance sampling data including the columns object, transect.ID, distance, 
#' x, y, true.species, unid, species, observer. The other items are the 
#' region, sample and observations tables as per the definitions in mrds.
#' @keywords datasets
NULL
