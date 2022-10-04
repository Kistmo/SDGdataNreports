####________________________________________________________________________________________
#---
# title: "ABR 10-14 datasets using DHS data"
# purpose: to ASFRs (ABRs) for SDG reporting
# author: "Stephen Kisambira"
# date: "26/03/2021"
# output: html_document, chart; excel file

# mrc-ide/hhsurveydata: Demographic analysis of DHS and other household surveys
# 
# https://rdrr.io/github/mrc-ide/hhsurveydata/
# 
# This package includes tools for calculating demographic indicators from household survey data. Initially developed for for processing and analysis from Demographic and Health Surveys (DHS) and Multiple Indicator Cluster Surveys (MICS). The package provides tools to calculate standard child mortality, adult mortality, and fertility indicators stratified arbitrarily by age group, calendar period, pre-survey time periods, birth cohorts and other survey variables (e.g. residence, region, wealth status, education, etc.). Design-based standard errors and sample correlations are available for all indicators via Taylor linearisation or jackknife.

# https://github.com/mrc-ide/demogsurv/blob/master/vignettes/rdhs-integration.pdf
# https://cran.r-project.org/web/packages/rdhs/vignettes/introduction.html

###


# House-keeping functions -------------------------------------------------
Sys.info()
####___________________________________________________________________________
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}

####______________________________________________________________________________
gc(); cat("\014"); clearhistory(); ##rm_history_database

####______________________________________________________________________________
# Round up from .5
## https://stackoverflow.com/questions/12688717/round-up-from-5
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}


# Libraries ---------------------------------------------------------------
####_________________________________________________________________________
# lapply(names(sessionInfo()$otherPkgs), function(pkgs)
#   detach(
#     paste0('package:', pkgs),
#     character.only = T,
#     unload = T,
#     force = T
#   ))
# 
####________________________________________________________________________________
.libPaths() 
.libPaths("/Library/Frameworks/R.framework/Versions/4.0/Resources/library") #for Mac

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## The easypackages package makes it easy to load or install multiple packages in R
# https://cran.r-project.org/web/packages/easypackages/README.html

# install.packages("easypackages")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#getCRANmirrors()
# Check if packages are installed (and install if not) in R:
# https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

## If a package is installed, it will be loaded. If any are not, the missing package(s) will be installed from CRAN and then loaded.

## First specify the packages of interest
my_packages = c("here", "rprojroot", "here", "httr", "jsonlite", "plotrix", "prettyR", "weights", "base64", "utils", "scales", "viridis", "Hmisc", "plyr", "dplyr", "reshape", "data.table", "magrittr", "tidyverse", "tidyr", "stats", "graphics", "FSA", "gtools", "foreign", "prettyGraphs", "prettyunits", "gmodels", "gdata", "gdata", "expss", "questionr", "hrbrthemes", "forcats", "ggplot2", "lattice", "png", "data.table", "rdhs", "demogsurv", "haven", "devtools", "rJava", "diffobj", "e1071", "ggeffects", "magick", "maptools", "phangorn", "processx", "RCurl", "sf", "survival", "tweenr", "units", "utf8", "XML", "jsonlite", "remotes", "httr", "demogsurv", "survey", "easypackages","RMySQL", "benchmarkme", "benchmarkmeData")

## Now load or install&load all
package.check <- lapply(
  my_packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Alternative loading or installing multiple packages:
#https://cran.r-project.org/web/packages/easypackages/easypackages.pdf
##packages("dplyr", "ggplot2", "RMySQL", "data.table", prompt = FALSE)
libraries(my_packages)
#packages(my_packages, prompt = FALSE)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## How to find all functions in an R package?
# https://stackoverflow.com/questions/20535247/how-to-find-all-functions-in-an-r-package
ls("package:easypackages")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## How should I deal with “package 'xxx' is not available (for R version x.y.z)” warning?
## https://stackoverflow.com/questions/25721884/how-should-i-deal-with-package-xxx-is-not-available-for-r-version-x-y-z-wa
#?setRepositories
#setRepositories()
##Return all the available packages using
ap <- available.packages()
View(ap)
"NCStats" %in% rownames(ap)

###+++++++++++++++
here::set_here(); here::dr_here(); here::here()

## A little nugget to retun API request as data.table rather than data.frame:
Sys.setenv(rdhs_DATA_TABLE = "TRUE")
## setwd("~/")
####________________________________________________________________________________

# Java --------------------------------------------------------------------
#Check Java issues:
system('java -version') # How to tell if you are running 64 or 32 bit Java

# Update packages ---------------------------------------------------------
update.packages(ask=F)

# Session info ------------------------------------------------------------

library(sessioninfo)
#sk ??sessionInfo
package_info("sessioninfo")
sessionInfo() #- Session information is invaluable because it captures all of the packages used in the current project.

# Directories (Working, Input, Utilities ----------------------------------
####_________________________________________________________________________________
#Analysis directory:
analysis.dir = NULL ##<< Analysis directory. If NULL, folder "fig" in current working directory.
if(is.null(analysis.dir)){
  analysis.dir <- file.path(here::here(), "Analysis//")
  dir.create(analysis.dir, showWarnings = FALSE)
} else {}

####_______________________________________________________________________________
#Input directory:
input.dir = NULL ##<<  If NULL, create in current working directory.
if(is.null(input.dir)){
  input.dir <- file.path(here::here(), "Input Data/")
  dir.create(input.dir, showWarnings = FALSE)
} else {}

####_____________________________________________________________________________
################### Output directory:
#Output directory:
output.dir = NULL ##<<  If NULL, create in current working directory.
if(is.null(output.dir)){
  output.dir <- file.path(here::here(), "Output Data/")
  dir.create(output.dir, showWarnings = FALSE)
} else {}

####________________________________________________________________________________
#MyUtils directory:
myutils.dir = NULL ##<< If NULL, create in current working directory.
if(is.null(myutils.dir)){
  myutils.dir <- file.path(here::here(), "MyUtils/")
  dir.create(myutils.dir, showWarnings = FALSE)
} else {}

####_______________________________________________________________________________

# Charts/figures directory:
fig.dir = NULL ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
if(is.null(fig.dir)){
  fig.dir <- file.path(here::here(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
} else {}

####______________________________________________________________________________
# Previous Versions directory:
previousversion.dir = NULL ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
if(is.null(previousversion.dir)){
  previousversion.dir <- file.path(here::here(), "Previous Versions/")
  dir.create(previousversion.dir, showWarnings = FALSE)
} else {}

####______________________________________________________________________________
# Previous figure directory:
previousfig.dir = NULL ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
if(is.null(previousfig.dir)){
  previousfig.dir <- file.path(paste(here::here(),"/fig", sep = ""), "Previous Versions/")
  dir.create(previousfig.dir, showWarnings = FALSE)
} else {}


# Data sets ---------------------------------------------------------------
# Read in the last file selected according to annual change in ABR  --------
here::here()
####_______________________________________________________________________________


library(haven)
UGIR61FL <- read_dta("UGIR61FL.DTA")
# View(UGIR61FL)

## Replicate DHS Table 5.1
## ASFR and TFR in 3 years preceding survey by residence
calc_asfr(UGIR61FL, ~1, tips=c(0, 3)) 
reshape2::dcast(calc_asfr(UGIR61FL, ~v025, tips=c(0, 3)), agegr ~ v025, value.var = "asfr")
calc_tfr(UGIR61FL, ~v025)
calc_tfr(UGIR61FL, ~1)

library(demogsurv)

calc_asfr(
  data,
  by = NULL,
  agegr = 3:10 * 5,
  period = NULL,
  cohort = NULL,
  tips = c(0, 3),
  clusters = ~v021,
  strata = ~v024 + v025,
  id = "caseid",
  dob = "v011",
  intv = "v008",
  weight = "v005",
  varmethod = "lin",
  bvars = grep("^b3\\_[0-9]*", names(data), value = TRUE),
  birth_displace = 1e-06,
  origin = 1900,
  scale = 12,
  bhdata = NULL,
  counts = FALSE,
  clustcounts = FALSE
)

countries <- dhs_countries()


data(zzir)
## Replicate DHS Table 5.1
## ASFR and TFR in 3 years preceding survey by residence
calc_asfr(zzir, ~1, tips=c(0, 3)) 
reshape2::dcast(calc_asfr(zzir, ~v025, tips=c(0, 3)), agegr ~ v025, value.var = "asfr")
calc_tfr(zzir, ~v025)
calc_tfr(zzir, ~1)


## Replicate DHS Table 5.2
## TFR by resdience, region, education, and wealth quintile
calc_tfr(zzir, ~v102)  # residence
calc_tfr(zzir, ~v101)  # region
calc_tfr(zzir, ~v106)  # education
calc_tfr(zzir, ~v190)  # wealth
calc_tfr(zzir)         # total

