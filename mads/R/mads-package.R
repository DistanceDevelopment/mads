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
#'  The main function in this pacakge is \link{execute.multi.analysis}.
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

