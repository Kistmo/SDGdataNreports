
####____________________________________________________________________________
# title: "DHS surveys conducted since 1984"
# purpose: Selection of  ASFRs (ABRs) for SDG reporting
# author: "Stephen Kisambira"
# date: "18/08/2022"
# output: html_document, chart; excel file

# House-keeping functions -------------------------------------------------
rm(list=ls()) #Clear all objects
Sys.info()

####___________________________________________________________________________
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}

#How to disable scientific notation?
# https://stackoverflow.com/questions/5352099/how-to-disable-scientific-notation
# https://stackoverflow.com/questions/53882326/setting-and-resetting-scipen-and-digits-in-r-options-with-package-settings
# You can turn it off with options(scipen = 999) and back on again with options(scipen = 0)

options(scipen=0, digits=7)

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

.libPaths() 
.libPaths("/Library/Frameworks/R.framework/Versions/4.1/Resources/library") #for Mac

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## The easypackages package makes it easy to load or install multiple packages in R
# https://cran.r-project.org/web/packages/easypackages/README.html

# install.packages("easypackages")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# source("http://www.rforge.net/NCStats/InstallNCStats.R")

#getCRANmirrors()

# Check if packages are installed (and install if not): -------------------
# https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
# https://stackoverflow.com/questions/26805267/r-subset-with-condition-using-in-or-which-one-should-be-used

## If a package is installed, it will be loaded. If any are not, the missing package(s) will be installed from CRAN and then loaded.

## First specify the packages of interest
my_packages = c("base64", "benchmarkme", "benchmarkme", "benchmarkmeData", "data.table", "demogsurv", "devtools", "diffobj", "dplyr", "e1071", "easypackages", "expss", "forcats", "foreign", "FSA", "gdata", "ggeffects", "ggplot2", "ggrepel", "ggtext", "gmodels", "graphics", "gsubfn", "gtools", "haven", "here", "Hmisc", "hrbrthemes", "httr", "jsonlite", "lattice", "magick", "magrittr", "maptools", "mgsub","nlme", "openssl", "openxlsx", "patchwork", "phangorn", "plotrix", "plotrix","prettyR", "plyr", "png", "prettyGraphs", "prettyR", "prettyunits", "processx", "questionr", "RCurl","readxl", "rdhs", "remotes", "rlist", "rJava", "RMySQL", "rprojroot", "sessioninfo", "scales", "scales", "sf", "sp", "stats", "stringr", "survey", "survival", "tictoc", "tidymodels", "tidyr", "tidyverse", "tweenr", "units", "utf8", "utils", "viridis", "weights", "XML")
#_________________________________________________________
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

search() # lists all parents of the global environment

# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Alternative loading or installing multiple packages:
#https://cran.r-project.org/web/packages/easypackages/easypackages.pdf
##packages("dplyr", "ggplot2", "RMySQL", "data.table", prompt = FALSE)
easypackages::libraries(my_packages)
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
#ap <- available.packages()
#View(ap)
#"NCStats" %in% rownames(ap)

###+++++++++++++++
here::set_here(); here::dr_here(); here::here()

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

####__________________________________________________________________________________

# Read in the most recent selceted data and remove outliers for selected countries --------
here::dr_here()
library(data.table)

# Read in data for plotting selected data points:
library(readxl)

#DHS surveys:
excel_sheets("Input Data/DHS_surveys.xlsx")

DHSsurveysByCountry.data <- read_excel("Input Data/DHS_surveys.xlsx", sheet = "SurveyCharacteristicsByCountry")

# names(DHSsurveysByCountry.data)

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###++++++++++++++ DHS surveys ++++++++++++++++++++++++++++++++++++
#Number of DHS surveys after Cairo POA in 1994
numberofdhsurveysafter1994.data <- DHSsurveysByCountry.data %>%
  subset(., Type=="Standard DHS" & Status=="Completed" & StartYear > 1994) %>%  
  #dplyr::distinct(LocName, .keep_all = TRUE) %>%
  unique(., by=c("LocName", "StartYear", "EndYear")) %>%
  dplyr::arrange(LocName, StartYear, EndYear)

## Extract Unique Elements:
DHScountriesafter1994.list <- numberofdhsurveysafter1994.data %>%  
  subset(., select = c(LocName)) %>%
  unique(., by=c("LocName")) %>%
  dplyr::arrange(LocName)

#Number of DHS surveys before Cairo POA in 1994
numberofdhsurveysbefore1994.data <- DHSsurveysByCountry.data %>%
  subset(., Type=="Standard DHS" & Status=="Completed" & StartYear < 1994) %>%  
  #dplyr::distinct(LocName, .keep_all = TRUE) %>%
  unique(., by=c("LocName", "StartYear", "EndYear")) %>%
  dplyr::arrange(LocName, StartYear, EndYear)

## Extract Unique Elements:
DHScountriesbefore1994.list <- numberofdhsurveysbefore1994.data %>%  
  subset(., select = c(LocName)) %>%
  unique(., by=c("LocName")) %>%
  dplyr::arrange(LocName)

numberofdhsurveysafter1994.data <- DHSsurveysByCountry.data %>%
  subset(., Type=="Standard DHS" & Status=="Completed" & StartYear > 1994)

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###++++++++++++++ MICS surveys ++++++++++++++++++++++++++++++++++++
#MICS surveys:
library(readxl)
excel_sheets("Input Data/MICS_surveys.xlsx")

MICSsurveysByCountry.data <- read_excel("Input Data/MICS_surveys.xlsx", 
                                        sheet = "surveys_catalogue")
# names(MICSsurveysByCountry.data)

#Number of MICS surveys after Cairo POA in 1994
levels(MICSsurveysByCountry.data$Status)
MICSsurveysByCountry.data$Status <- as.factor(as.character(MICSsurveysByCountry.data$Status))

numberofMICSurveysafter1994.data <- MICSsurveysByCountry.data %>%
  subset(., Status=="Completed"  & Sample=="Nationally representative"
         & StartYear > 1994) %>%  
  #dplyr::distinct(LocName, .keep_all = TRUE) %>%
  unique(., by=c("LocName", "StartYear", "EndYear")) %>%
  dplyr::arrange(LocName, StartYear, EndYear)

## Extract Unique Elements:
MICScountriesafter1994.list <- numberofMICSurveysafter1994.data %>%  
  subset(., select = c(LocName)) %>%
  unique(., by=c("LocName")) %>%
  dplyr::arrange(LocName)

#Number of MICS surveys before Cairo POA in 1994
numberofMICSurveysbefore1994.data <- MICSsurveysByCountry.data %>%
  subset(., Status=="Completed"  & Sample=="Nationally representative"
         & StartYear < 1994) %>%  
  #dplyr::distinct(LocName, .keep_all = TRUE) %>%
  unique(., by=c("LocName", "StartYear", "EndYear")) %>%
  dplyr::arrange(LocName, StartYear, EndYear)

## Extract Unique Elements:
MICScountriesbefore1994.list <- numberofMICSurveysbefore1994.data %>%  
  subset(., select = c(LocName)) %>%
  unique(., by=c("LocName")) %>%
  dplyr::arrange(LocName)
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





