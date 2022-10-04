
####____________________________________________________________________________
# title: "Functions for selecting ABR (10-14 and 15-19 years) for SDG reporting"
# purpose: Select ASFRs (ABRs) for SDG reporting
# author: "Stephen Kisambira"
# date: "16/08/2022"
# output: html_document, chart; excel file

# House-keeping functions -------------------------------------------------
rm(list=ls()) #Remove all objects.
###+++++++++++++++
here::set_here(); here::dr_here(); here::here()
####____________________________________________________________________________

#https://stackoverflow.com/questions/62592680/r-argument-to-answer-no-to-install-from-source-that-require-compilation
options("install.packages.compile.from.source" = "never")
Sys.info()

# R citation: -------------------------------------------------------------
citation()
library(rstudioapi)
versionInfo()$citation

####___________________________________________________________________________
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}

# How to disable scientific notation? -------------------------------------
# https://stackoverflow.com/questions/5352099/how-to-disable-scientific-notation
# https://stackoverflow.com/questions/53882326/setting-and-resetting-scipen-and-digits-in-r-options-with-package-settings
# You can turn it off with options(scipen = 999) and back on again with options(scipen = 0)
options(scipen=999, digits=2)

# Round up from .5 function -----------------------------------------------
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
.libPaths() 
.libPaths("/Library/Frameworks/R.framework/Versions/4.1/Resources/library") #for Mac

# Packages ----------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## The easypackages package makes it easy to load or install multiple packages in R
# https://cran.r-project.org/web/packages/easypackages/README.html
# install.packages("easypackages")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# source("http://www.rforge.net/NCStats/InstallNCStats.R")
#getCRANmirrors()

####--------- Check if packages are installed (and install if not):
# https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
# https://stackoverflow.com/questions/26805267/r-subset-with-condition-using-in-or-which-one-should-be-used

## If a package is installed, it will be loaded. If any are not, the missing package(s) will be installed from CRAN and then loaded.

## First specify the packages of interest
my_packages = c("abd", "ape", "base", "base64", "benchmarkme", "benchmarkmeData", "broom", "data.table", "datasets", "DBI", "demogsurv", "devtools", "DiagrammeR", "DiagrammeRsvg", "dials", "diffobj", "downloader", "dplyr", "e1071", "easypackages", "expss", "forcats", "foreign", "Formula", "FSA", "gdata", "ggalt", "ggeffects", "ggformula", "ggplot2", "ggrepel", "ggridges", "ggstance", "ggtext", "gmodels", "graphics", "grDevices", "grid", "gsubfn", "gtools", "haven", "here", "Hmisc", "hrbrthemes", "httr", "infer", "influenceR", "jsonlite", "lattice", "magick", "magrittr", "maptools", "Matrix", "methods", "mgsub", "modeldata", "mosaic", "mosaicCore", "mosaicData", "naniar", "nlme", "openssl", "openxlsx", "parsnip", "patchwork", "phangorn", "plotrix", "plyr", "png", "prettyGraphs", "prettyR", "prettyunits", "processx", "proto", "purrr", "questionr", "ragg", "rappdirs","RCurl", "rdhs", "readr", "recipes", "remotes", "reshape", "rJava", "rlist", "RMySQL", "rprojroot", "rsample", "rstudioapi", "scales", "sessioninfo", "sf", "sp", "stats", "stringr", "survey", "survival", "tables", "tibble", "tictoc", "tidymodels", "tidyr", "tidystats", "tidyverse", "tigerstats", "timevis", "tools", "tune", "tweenr", "units", "usethis", "utf8", "utils", "viridis", "viridisLite", "visNetwork", "vtree", "weights", "workflows", "workflowsets", "wrapr", "XML", "yardstick")
#_________________________________________________________
#??tidystats

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

dput(search()) # lists all parents of the global environment

# Alternative loading or installing multiple packages: --------------------
#https://cran.r-project.org/web/packages/easypackages/easypackages.pdf
##packages("dplyr", "ggplot2", "RMySQL", "data.table", prompt = FALSE)
easypackages::libraries(my_packages)
#packages(my_packages, prompt = FALSE)

# Update packages ---------------------------------------------------------
update.packages(ask=FALSE)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## How to find all functions in an R package?
# https://stackoverflow.com/questions/20535247/how-to-find-all-functions-in-an-r-package
ls("package:easypackages")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# How should I deal with “package 'xxx' is not available (for R ve --------
## How should I deal with “package 'xxx' is not available (for R version x.y.z)” warning?
## https://stackoverflow.com/questions/25721884/how-should-i-deal-with-package-xxx-is-not-available-for-r-version-x-y-z-wa
#?setRepositories
#setRepositories()
##Return all the available packages using
#ap <- available.packages()
#View(ap)
#"NCStats" %in% rownames(ap)

# Session info ------------------------------------------------------------
library(sessioninfo)
#sk ??sessionInfo
package_info("sessioninfo")
sessionInfo() #- Session information is invaluable because it captures all of the packages used in the current project.

# Directories -------------------------------------------------------------
# library(here)
# here::set_here(); here::dr_here(); here::here()
# library(conflicted)
# here()
# #here::i_am("abr-selection-functions.R")
getwd()
####________________________________________________________________________________

# Java --------------------------------------------------------------------
#Check Java issues:
system('java -version') # How to tell if you are running 64 or 32 bit Java




##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## How to find all functions in an R package?
# https://stackoverflow.com/questions/20535247/how-to-find-all-functions-in-an-r-package
ls("package:easypackages")

####___________________________________________________________________________
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}


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
previousversion.dir = NULL ##<< Directory to store overview plots. If NULL, folder "previousversion.dir" in current working directory.
if(is.null(previousversion.dir)){
  previousversion.dir <- file.path(here::here(), "Previous Versions/")
  dir.create(previousversion.dir, showWarnings = FALSE)
} else {}

####______________________________________________________________________________
# Previous R versions directory:
previousRversion.dir = NULL
if(is.null(previousRversion.dir)) {
  previousRversion.dir <- file.path(here::here(), "Previous R Files")
  dir.create(previousRversion.dir, showWarnings = FALSE)
} else {}

####______________________________________________________________________________
# Previous figure directory:
previousfig.dir = NULL ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
if(is.null(previousfig.dir)){
  previousfig.dir <- file.path(paste(here::here(),"/fig", sep = ""), "Previous Versions/")
  dir.create(previousfig.dir, showWarnings = FALSE)
} else {}





# Function to plot the graph and output selected data ---------------------
abr1014.function <- try(function(x){

############### Read a set of data files for a country into R ################################
############  (http://stats.idre.ucla.edu/r/codefragments/read_multiple/) ####################

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #   rm(list=ls(pattern="gg|^Count|^kk|^datas|^dd|^p|^docount|^mm|^p|^ap|^abr.c"))

  
    CountryID = x
  # CountryID <- "Afghanistan"
  # CountryID <- "Bangladesh"
  # CountryID <- "Uganda"    
    docountry <- subset(ABR_AllDatapoints2022.data, LocName==CountryID)
    docountry1 <- subset(AllDataSourcesDemoData.data, LocName==CountryID)    
    docountry2 <- subset(MaxDataSourceSeries.data, LocName==CountryID)    
    docountry3 <- subset(ABR_SDG2021.data, LocName==CountryID)

        #Select data for Bangladesh example:
    names(docountry)
    library(magrittr)
      if(docountry$LocName[1]=="Bangladesh"){
        subset(docountry, !(DataCatalogShortName=="SVRS"  & RefYear < 2016)) %>%
        subset(., !(DataCatalogShortName=="SVRS"  & RefYear == 2016 & ABR < 83))
      } -> docountry
    
#### Replace specified values in a vector or factor with new values. ##### 
      if(docountry$LocName[1]=="Bangladesh"){
        docountry$DataCatalogShortName <- plyr::mapvalues(docountry$DataCatalogShortName,
                                     from=c("2019 SVRS"),
                                     to = c("SVRS")) }
    
# docountry1$DataCatalogShortName <- plyr::mapvalues(docountry1$DataCatalogShortName,
#                                      from=c("2019 SVRS"),
#                                      to = c("SVRS"))    

    ###____________________________________________________________________

# if(nrow(docountry) < 1){ #If docountry has no observations, skip 
#       } else { 
###_______________________________________________________________________________        
gc(); cat("\014"); clearhistory(); #rm_history_database
###_______________________________________________________________________________

#https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html

names(docountry)
# Remove unwanted parts of column name after certain characters (to be used in chart legend):
# # https://stackoverflow.com/questions/37800704/r-remove-parts-of-column-name-after-certain-characters
#names(DataCatalog) = gsub(pattern = "*.x", replacement = "", x = names(DataCatalog))
##colnames(docountry) <- sub("_1014", "", colnames(docountry))


DataSourcesChart.fn <- function(docountry, RefYear, ABR, DataCatalogShortName) {
  docountry %>%
    # ggplot( aes(x=RefYear, y=ABR, color = factor(RefYear), fill=ABR)) +
    ggplot( aes(x=RefYear, y=ABR)) +
      geom_line(aes(linetype=DataCatalogShortName, color = DataCatalogShortName), alpha=0.9, size=1.1) +    
    #geom_point()  +
    geom_point(aes(color=DataCatalogShortName), size=3.5) + 
    geom_point(mapping = aes(color = DataCatalogShortName)) +  
          guides(color = "none", linetype = "none") +
    # geom_text_repel(aes(y = ABR, label = DataCatalogShortName, vjust = -1)) +
    #https://stackoverflow.com/questions/43023569/how-to-give-multiple-conditions-in-geom-text
    # geom_text(aes(label=ifelse((SDG2020Status=="Alternative" | SDG2020Status=="NEW"), ABRcat, "")), vjust = -0.7, hjust = -0.1) +
    theme_bw() +
    #https://stackoverflow.com/questions/10861773/remove-grid-background-color-and-top-and-right-borders-from-ggplot2
    theme(axis.line = element_line(colour = "black"),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          strip.background = element_rect(
            color="black", fill="white", size=0.8, linetype="solid")) +
    #https://stackoverflow.com/questions/30083002/ggplot-legend-list-is-larger-than-page  
     theme(
      legend.position="top", legend.direction = "vertical",
      panel.spacing = unit(0.1, "lines"), legend.margin=margin(),
      strip.text.x = element_text(size = 8), legend.title = element_blank(),
      legend.key.size=unit(1,"point")) +
    guides(colour=guide_legend(nrow=5)) + 
    #guides(colour=guide_legend(nrow=1)) + 
    # guides(fill = FALSE, size = FALSE) +
    guides(fill = "none", size = "none") +    
    ggtitle(paste(docountry$LocName[1],": All data points", sep = "")) +
    theme(plot.title = element_text(size=12, hjust = 0.5)) +
    theme(axis.title.x = element_text(size=10, hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    theme(axis.title.y = element_text(size=10, hjust = 0.5)) +
      xlab("Reference Year") +
      #ylab("ABR 10-14") +
    # geom_smooth(method=loess, se=TRUE, level = 0.95, span = 0.75,
    #             fullrange=TRUE, inherit.aes = TRUE) +
  scale_x_continuous(breaks = docountry$RefYear, guide = guide_axis(n.dodge = 2)) +
    #scale_y_continuous(breaks=seq(0, k, 20)) +
    #Set the maximum y-value to a factor of 1.25:
    coord_cartesian(ylim = c(0, max(docountry$ABR)*1.25), expand = TRUE)
}

###++++++++++++++++ End of DataSourcesChart.fn Function ++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++__________+++++++++++++++++
#Data based on all data sources:
#ABR 10-14:
names(docountry)

docountry1a <-  docountry %>%
  filter(AgeStart==10)
docountry1b <-  docountry %>%
  filter(AgeStart==15)

#___________________________________
# Change docountry1a$DataCatalogShortName to factor before changing its labels:
# How to convert a factor to an integer\numeric without a loss of information?
# https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
#str(docountry1a)
names(docountry1a)
docountry1a$DataCatalogShortName <- as.factor(as.character(docountry1a$DataCatalogShortName))
docountry1b$DataCatalogShortName <- as.factor(as.character(docountry1b$DataCatalogShortName))

#+++++++++++__________++++++++++++++++
#+++++++++++__________+++++++++++++++++
#Data based on Same data source type:
#ABR 10-14:

docountry1c <-  docountry1 %>%
  filter(AgeStart==10)
docountry1d <-  docountry1 %>%
  filter(AgeStart==15)

docountry1c$DataCatalogShortName <- as.factor(as.character(docountry1c$DataCatalogShortName))
docountry1d$DataCatalogShortName <- as.factor(as.character(docountry1d$DataCatalogShortName))
#___________________________________
#+++++++++++__________++++++++++++++++
#___________________________________
#Data from SDG 2021:
#ABR 15-19:

docountry3b <-  docountry3  #%>%
#filter(AgeStart==15)

docountry3b$DataCatalogShortName <- as.factor(as.character(docountry3b$DataCatalogShortName))
#___________________________________
#Combine levels for ABR 10-14 with levels for ABR 15-19 to have similar legend labels:
drop.levels(docountry1a$DataCatalogShortName)
drop.levels(docountry1b$DataCatalogShortName)
drop.levels(docountry1a$DataCatalogShortName) %in% drop.levels(docountry1b$DataCatalogShortName)
kk1a <- drop.levels(docountry1a$DataCatalogShortName)
kk1b <- drop.levels(docountry1b$DataCatalogShortName)
kk1c <- drop.levels(docountry1c$DataCatalogShortName)

# kk1c <- levels(docountry1c$DataCatalogShortName)
kk1d <- drop.levels(docountry1d$DataCatalogShortName)

setdiff(kk1a, kk1b)
setdiff(kk1a, kk1b)
union(kk1a, kk1b)
union(kk1a, kk1c)
union(kk1c, kk1d)
#___________________________________
#+++++++++++__________+++++++++++++++++

####++++++++++++++++++ Plot the charts +++++++++++++++++++++++++++
# You can add plots saved to variables:
library(ggplot2)
#str(docountry1a)

# ABR 10-14 based on all data sources
# Select the ABR 10-14 data that would be selected based on the selected ABR 15-19:
docountry1a_select <- subset(docountry1a, docountry1a$RefYear %in% docountry3b$RefYear) %>%
  mutate(DataCatalogShortName1="Blue dashed line =\nselection based on\n reported ABR 15-19 in 2021")

#Plot chart:
if( nrow(docountry1a) < 1 ) {
  #skip
} else {
p1a <- DataSourcesChart.fn(docountry1a) +
    ggtitle(paste(docountry1a$LocName[1],": All data sources,\n ABR 10-14", sep = "")) +
  ylab("ABR 10-14") +
       scale_colour_discrete(drop=TRUE,
         limits = union(kk1b, kk1a))  +
  #Add plot from another source:
   #geom_line(data = docountry1a_select, colour="red", size=0.9)  +
   geom_line(data = docountry1a_select, linetype = "dashed", color="blue", alpha=0.9, size=1.2) +
  # geom_label(aes(label = DataCatalogShortName1), data = docountry1a_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -4.35, nudge_y = 12.65, size = 3) +
  geom_label(aes(label = DataCatalogShortName1), data = docountry1a_select %>%
               filter(RefYear == max(RefYear)), nudge_x = -8.35, nudge_y = 20.65, size = 3) +
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
        }

if( nrow(docountry1b) < 1 ) {
  #skip
} else {
p1b <- DataSourcesChart.fn(docountry1b) +
    ggtitle(paste(docountry1b$LocName[1],": All data sources,\n ABR 15-19", sep = "")) +
      ylab("ABR 15-19")  + 
# #https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin/6924503#6924503:
       # scale_colour_discrete(drop=TRUE,
       #   limits = levels(docountry1c$DataCatalogShortName))
     scale_colour_discrete(drop=TRUE,
         limits = union(kk1b, kk1a))  +
  #Add plot from another source:
   geom_line(data = docountry3b, colour="blue", size=0.6)  +
  # geom_label(aes(label = DataCatalogShortName1), data = docountry3b_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -4.35, nudge_y = 12.65, size = 3) +
  # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -10.35, nudge_y = -30.65, size = 3) +
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
}

#___________________________________

# ABR 10-14 based on same data source type
# Select the ABR 10-14 data that have same data type as the ABR 15-19  data:
docountry1c <- subset(docountry1c, docountry1c$DataProcessType %in% docountry1d$DataProcessType)
# Select the ABR 10-14 data that have same data type as the ABR 15-19  data:
docountry1d <- subset(docountry1d, docountry1d$DataProcessType %in% docountry1c$DataProcessType)
# Select the ABR 10-14 data that would be selected based on the selected ABR 15-19:
docountry1c_select <- subset(docountry1c, docountry1c$RefYear %in% docountry3b$RefYear) %>%
  mutate(DataCatalogShortName1="Blue dashed line =\nselection based on\n reported ABR 15-19 in 2021")

names(docountry1c)
#Plot chart:
if( nrow(docountry1c) < 1 ) {
  #skip
} else {
p1c <- DataSourcesChart.fn(docountry1c) +
    ggtitle(paste(docountry1c$LocName[1],": Same data source type,\n ABR 10-14", sep = "")) +
  ylab("ABR 10-14") +
       scale_colour_discrete(drop=TRUE,
         limits = union(kk1c, kk1d))  +
  #Add plot from another source:
   #geom_line(data = docountry1c_select, colour="red", size=0.9) +
   geom_line(data = docountry1c_select, linetype = "dashed", color="blue", alpha=0.9, size=1.2) + 
  # geom_label(aes(label = DataCatalogShortName1), data = docountry1c_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -4.35, nudge_y = 12.65, size = 3) +
  geom_label(aes(label = DataCatalogShortName1), data = docountry1c_select %>%
               filter(RefYear == max(RefYear)), nudge_x = -9.35, nudge_y = 12.65, size = 3) +
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE) 
      }

#ABR 15-19:
if( nrow(docountry1d) < 1 ) {
  #skip
} else {
p1d <- DataSourcesChart.fn(docountry1d) +
    ggtitle(paste(docountry1d$LocName[1],": Same data source type,\n ABR 15-19", sep = "")) +
      ylab("ABR 15-19") + 
# #https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin/6924503#6924503:
       # scale_colour_discrete(drop=TRUE,
       #   limits = levels(docountry1c$DataCatalogShortName))
     scale_colour_discrete(drop=TRUE,
         limits = union(kk1c, kk1d))  +
  #Add plot from another source:
   geom_line(data = docountry3b, colour= "blue", size=0.9) +
  # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -1.35, nudge_y = -7.65, size = 3) +
  # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -9.35, nudge_y = -30.65, size = 3) +  
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=FALSE, inherit.aes = TRUE)
}
#_________________________________________________________________
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++__________+++++++++++++++++
#Data based on preponderant type of data source:

names(docountry2)
docountry2a <-  docountry2 %>%
  filter(AgeStart==10)
#___________________________________
#Data based on preponderant data sources:
#ABR 15-19:
docountry2b <-  docountry2 %>%
  filter(AgeStart==15)
#______________________________________
# Change docountry2a$DataCatalogShortName to factor before changing its labels:
# How to convert a factor to an integer\numeric without a loss of information?
# https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
#str(docountry2a)
names(docountry2a)
#docountry2$DataCatalogShortName <- as.factor(as.character(docountry2$DataCatalogShortName))
docountry2a$DataCatalogShortName <- as.factor(as.character(docountry2a$DataCatalogShortName))
docountry2b$DataCatalogShortName <- as.factor(as.character(docountry2b$DataCatalogShortName))
#str(docountry2a)
#+++++++++++__________+++++++++++++++++

#___________________________________

#ABR 10-14:
# ABR 10-14 based on preponderant type of data source;
# Select the ABR 10-14 data that would be selected based on the selected ABR 15-19:
docountry2a_select <- subset(docountry2a, docountry2a$RefYear %in% docountry3b$RefYear) %>%
  mutate(DataCatalogShortName1="Blue dashed line =\nselection based on\n reported ABR 15-19 in 2021")

# Select the ABR 10-14 data that would be selected based on the selected ABR 15-19:
docountry2b_select <- subset(docountry2b, docountry2a$RefYear %in% docountry3b$RefYear) %>%
  mutate(DataCatalogShortName1="Blue solid line =\nselected ABR 15-19\n reported in 2021")

#Plot chart:

#______________________________________
#Identify type of data source:
#https://stackoverflow.com/questions/1740524/count-number-of-objects-in-list/21444908
if( nrow(docountry2a) < 1 ) {
  #skip
} else {
tds2a <- unique(docountry2a$DataProcessType) 
datasourcetype2a <- ifelse(length(tds2a) == 2, tolower(paste(tds2a[[1]]," and ", tds2a[[2]], sep = "")), tolower(paste(tds2a[[1]]))); rm(tds2a)

p2a <- DataSourcesChart.fn(docountry2a) +
    ggtitle(paste(docountry2a$LocName[1],": Preponderant data\n source (",datasourcetype2a, "),\n ABR 10-14", sep = "")) +
  ylab("ABR 10-14") + 
#https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin/6924503#6924503:
       scale_colour_discrete(drop=TRUE,
         limits = levels(docountry2a$DataCatalogShortName))  +
  #Add plot from another source:
   #geom_line(data = docountry2a_select, colour="red", size=0.9) +
   geom_line(data = docountry2a_select, linetype = "dashed", color="blue", alpha=0.9, size=1.2) +
  # geom_label(aes(label = DataCatalogShortName1), data = docountry2a_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -4.35, nudge_y = 12.65, size = 3) +
  geom_label(aes(label = DataCatalogShortName1), data = docountry2a_select %>%
               filter(RefYear == max(RefYear)), nudge_x = -7.35, nudge_y = 12.65, size = 3) +  
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE) 
      }

if( nrow(docountry2b) < 1 ) {
  #skip
} else {
tds2b <- unique(docountry2b$DataProcessType) 
datasourcetype2b <- ifelse(length(tds2b) == 2, tolower(paste(tds2b[[1]]," and ", tds2b[[2]], sep = "")), tolower(paste(tds2b[[1]]))); rm(tds2b)

p2b <- DataSourcesChart.fn(docountry2b) +
    ggtitle(paste(docountry2b$LocName[1],": Preponderant data\n source (",datasourcetype2b, "),\n ABR 15-19", sep = "")) +
      ylab("ABR 15-19") + 
#https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin/6924503#6924503:
       scale_colour_discrete(drop=TRUE,
         limits = levels(docountry2b$DataCatalogShortName))  +
  #Add plot from another source:
  geom_line(data = docountry2b_select, linetype = "solid", color="blue", alpha=0.9, size=0.6) +  
  #geom_line(data = docountry3b, colour= "blue", size=0.9) +
  # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -1.35, nudge_y = -7.65, size = 3) +
  geom_label(aes(label = DataCatalogShortName1), data = docountry2b_select %>%
               filter(RefYear == max(RefYear)), nudge_x = -9.35, nudge_y = -30.65, size = 3) +
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
}

#______________________________________
#_________________________________________________________________
if( exists('p1a') & exists('p1b') ) {
  print(p1a + p1b)
} else {
    #skip
  }


if( exists('p1c') & exists('p1d') ) {
  print(p1c + p1d)
} else {
    #skip
  }

if( exists('p2a') & exists('p2b') ) {
  print(p2a + p2b)
} else {
    #skip
  }
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#rm(list=ls(pattern="^p1|^p2|^kk1"))

# rm(list=ls(pattern="^Bangl|^Countr|^gg|^dataso|^dd|^p|^docount|^kk|^mm|^p|^ap|^abr.c|^df_|^gb"))
## objects()
#rm(docountry, Alldatapoints.chart)
      # }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

####_________________________________________________________________________
  gc(verbose = getOption("verbose"), reset = TRUE) # Do a garbage collection        
####_________________________________________________________________________
  
} , silent = TRUE)


###_______________________________________________________________________________
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
###_______________________________________________________________________________
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# SELECT DATA AND OUTPUT THEM TO EXCEL FILE -------------------------------

###_______________________________________________________________________________
#@@@@@@@@@@@@@ FUNCTION TO SELECT DATA AND OUTPUT THEM TO EXCEL FILE	@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
####_______________________________________________________________________________
#  rm(list=ls(pattern="^df_new|^outputl|^p1|^p2|^docountry"))

# Function to select data and output them to Excel file ---------------------
countrydata.function <- try(function(x){

############### Read a set of data files for a country into R ################################
############  (http://stats.idre.ucla.edu/r/codefragments/read_multiple/) ####################

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    CountryID = x
  # CountryID <- "Afghanistan"
  # CountryID <- "Bangladesh"
    # CountryID <- "Albania" 
    # CountryID <- "Algeria"    
    # CountryID <- "American Samoa" 
    #  CountryID <- "Côte d'Ivoire"
    # CountryID <- "Antigua and Barbuda"
    # CountryID <- "Viet Nam"
    docountry <- subset(AllDataSourcesDemoData.data, LocName==CountryID)
    docountry1 <- subset(AllDataSourcesDemoData.data, LocName==CountryID)
    docountry2 <- subset(MaxDataSourceSeries.data, LocName==CountryID)    
    docountry3 <- subset(ABR_SDG2021.data, LocName==CountryID)

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++__________+++++++++++++++++
#Data based on all data sources:
#ABR 10-14:
names(docountry1)

docountry1a <-  docountry1 %>%
  #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    dplyr::rename_all(
    ~stringr::str_replace_all(., "_1014", "")
    ) %>%
  group_by(LocID, RefYear) %>%
    arrange(RefYear, RecallLag, ABR1014) %>%
    dplyr::distinct(RefYear, !is.na(ABR1014), .keep_all = T)  %>%
    filter(!is.na(ABR1014)) %>%
  ungroup() %>% 
  plyr::rename(., c("ABR1014" = "ABR"))
#___________________________________
# Change docountry1a$DataCatalogShortName to factor before changing its labels:
# How to convert a factor to an integer\numeric without a loss of information?
# https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
#str(docountry1a)
names(docountry1a)
docountry1a$DataCatalogShortName <- as.factor(as.character(docountry1a$DataCatalogShortName))
#str(docountry1a)


####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#___________________________________
#+++++++++++__________++++++++++++++++
#Data based on all data sources:
#ABR 15-19:

docountry1b <-  docountry1 %>%
    #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    dplyr::rename_all(
    ~stringr::str_replace_all(., "_1519", "")
    ) %>%
  group_by(LocID, RefYear) %>%
    arrange(RefYear, RecallLag, ABR1519) %>%
    dplyr::distinct(RefYear, !is.na(ABR1519), .keep_all = T)  %>%
    filter(!is.na(ABR1519)) %>%
  ungroup() %>% 
      plyr::rename(., c("ABR1519" = "ABR"))

docountry1b$DataCatalogShortName <- as.factor(as.character(docountry1b$DataCatalogShortName))

#___________________________________
#+++++++++++__________++++++++++++++++
#Data based on Same data source type:(same as docountry1a)
#ABR 10-14:

docountry1c <-  docountry1 %>%
    #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    dplyr::rename_all(
    ~stringr::str_replace_all(., "_1014", "")
    ) %>%
  group_by(LocID, RefYear) %>%
    arrange(RefYear, RecallLag, ABR1014) %>%
    dplyr::distinct(RefYear, !is.na(ABR1014), .keep_all = T)  %>%
    filter(!is.na(ABR1014)) %>%
  ungroup() %>% 
      plyr::rename(., c("ABR1014" = "ABR"))

docountry1c$DataCatalogShortName <- as.factor(as.character(docountry1c$DataCatalogShortName))

#___________________________________
#+++++++++++__________++++++++++++++++
docountry1d <-  docountry1 %>%
  group_by(LocID, RefYear) %>%
    #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    dplyr::rename_all(
    ~stringr::str_replace_all(., "_1519", "")
    ) %>%  
    arrange(RefYear, RecallLag, ABR1519) %>%
    dplyr::distinct(RefYear, !is.na(ABR1519), .keep_all = T)  %>%
    filter(!is.na(ABR1014) & !is.na(ABR1519)) %>%
  ungroup() %>% 
      plyr::rename(., c("ABR1519" = "ABR"))

docountry1d$DataCatalogShortName <- as.factor(as.character(docountry1d$DataCatalogShortName))

#___________________________________
#+++++++++++__________++++++++++++++++
#Data from SDG 2021:
#ABR 15-19:

docountry3b <-  docountry3 %>%
    #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    dplyr::rename_all(
    ~stringr::str_replace_all(., "_1519", "")
    ) %>%  
  group_by(LocID, RefYear) %>%
    arrange(RefYear, ABR1519) %>%
    dplyr::distinct(RefYear, !is.na(ABR1519), .keep_all = T)  %>%
    filter(!is.na(ABR1519)) %>%
  ungroup() %>% 
      plyr::rename(., c("ABR1519" = "ABR"))

docountry3b$DataCatalogShortName <- as.factor(as.character(docountry3b$DataCatalogShortName))

#___________________________________
#+++++++++++__________++++++++++++++++
#Data based on preponderant type of data source:

names(docountry2)
docountry2a <-  docountry2 %>%
    #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    dplyr::rename_all(
    ~stringr::str_replace_all(., "_1014", "")
    ) %>%  
  group_by(LocID, RefYear) %>%
    arrange(RefYear, RecallLag, ABR1014) %>%
    dplyr::distinct(RefYear, !is.na(ABR1014), .keep_all = T)  %>%
    filter(!is.na(ABR1014)) %>%
  ungroup() %>% 
      plyr::rename(., c("ABR1014"="ABR")) %>%
  select(.,-c(`!is.na(ABR1014)`))

#___________________________________
#+++++++++++__________++++++++++++++++
#Data based on preponderant data sources:
#ABR 15-19:
docountry2b <-  docountry2 %>%
    #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    dplyr::rename_all(
    ~stringr::str_replace_all(., "_1519", "")
    ) %>%    
  group_by(LocID, RefYear) %>%
    arrange(RefYear, RecallLag, ABR1519) %>%
    dplyr::distinct(RefYear, !is.na(ABR1519), .keep_all = T)  %>%
    filter(!is.na(ABR1519)) %>%
  ungroup() %>% 
      plyr::rename(., c("ABR1519"="ABR")) %>%
  select(.,-c(`!is.na(ABR1519)`))

#___________________________________
#+++++++++++__________++++++++++++++++
# Change docountry2a$DataCatalogShortName to factor before changing its labels:
# How to convert a factor to an integer\numeric without a loss of information?
# https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
#str(docountry2a)
names(docountry2a)
#docountry2$DataCatalogShortName <- as.factor(as.character(docountry2$DataCatalogShortName))
docountry2a$DataCatalogShortName <- as.factor(as.character(docountry2a$DataCatalogShortName))
docountry2b$DataCatalogShortName <- as.factor(as.character(docountry2b$DataCatalogShortName))
#str(docountry2a)
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Function to select data that fall within the CI of 95%: -----------------

# Find points over and under the confidence interval when using geom_stat / geom_smooth in ggplot2
# https://stackoverflow.com/questions/33082901/find-points-over-and-under-the-confidence-interval-when-using-geom-stat-geom-s

library(sp)

rara.function <- function(countrydata, RefYear, ABR, ggplot2, ggeffects, in_ci, ., Within_ci, DataCatalogShortName) {
  df <- countrydata
  # we have to build the plot first so ggplot can do the calculations
  ggplot(df,aes(RefYear, ABR)) +
    geom_point() +
    #geom_smooth(  se = TRUE, level = 0.80) -> gg +
    #geom_smooth(  se = TRUE, level = 0.90) -> gg #+
    geom_smooth(  se = TRUE, na.rm = TRUE, level = 0.95) -> gg
  
  # do the calculations
  gb <- ggplot_build(gg)
  
  # get the CI data
  p <- gb$data[[2]]
  
  rm(gg,gb)
  
  ####________________________________________________
  if(nrow(p) < 1){df <- mutate(df, in_ci="NA",	Within_ci="NA")} else {
    # make a polygon out of it
    
    poly <- data.frame(
      x=c(p$x[1],    p$x,    p$x[length(p$x)],    rev(p$x)), 
      y=c(p$ymax[1], p$ymin, p$ymax[length(p$x)], rev(p$ymax))
    )
    
    # test for original values in said polygon and add that to orig data
    # so we can color by it
    
    df$in_ci <- point.in.polygon(df$RefYear, df$ABR, poly$x, poly$y)
    
    rm(p, poly)
    
    # It needs a bit of tweaking (i.e that last point getting a 2 value) but I'm limited on time. NOTE that the point.in.polygon return values are:
    # 
    # 0: point is strictly exterior to pol
    # 1: point is strictly interior to pol
    # 2: point lies on the relative interior of an edge of pol
    # 3: point is a vertex of pol
    # so it should be easy to just change the code to TRUE/FALSE whether value is 0 or not.
    
    library(ggplot2)
    library(ggeffects)
    
    df <- mutate(df, Within_ci=ifelse(in_ci==0, "Outside CI", "Inside CI") ) %>% 
      subset(., !in_ci==0)
    
    levels(factor(df$in_ci))
    # names(df)
    
    # re-do the plot with the new data
    if(nrow(df) < 1){} else {
      ggplot(df,aes(RefYear, ABR)) +
        geom_point(aes(color=factor(in_ci), size = 0.5)) +
        #geom_smooth() +
        geom_line(linetype = "dashed", color="blue", alpha=0.9, size=1.2)  +
        #https://stackoverflow.com/questions/43023569/how-to-give-multiple-conditions-in-geom-text
        geom_text(aes(label=ifelse((Within_ci=="Outside CI"), DataSourceShortName, ""), vjust = -1))
    }
    #https://stackoverflow.com/questions/15994368/how-to-make-object-created-within-function-usable-outside
    return(df)
  }
  
  # rm(df)
  gc(); cat("\014"); clearhistory(); #rm_history_database
  
  ##++++ Extract data from a ggplot +++++##
  ## https://www.py4u.net/discuss/859372
  
}

####++++++++++++++  End of rara.funtion +++++++++++++++++++++++++++++
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Output data -------------------------------------------------------------

#All data sources: 10-14
#Create output data:
#rara.function(docountry1a)
mm <- rara.function(docountry1a)

if(is.null(mm)){} else {  
  #Create output data:
  df_new1a <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    select(., -contains(c("X.is.na.", "!is.na(")))  %>%
    dplyr::mutate(Source = "All data sources"); rm(mm)
}


#All data sources: 15-19
#Create output data:
#rara.function(docountry1b)
mm <- rara.function(docountry1b)

if(is.null(mm)){} else {  
  df_new1b <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    select(., -contains(c("X.is.na.", "!is.na(")))  %>%
    dplyr::mutate(Source = "All data sources"); rm(mm)
}

##+++++++++++
#Same data source type: 10-14
#Create output data:
#rara.function(docountry1c)
mm <- rara.function(docountry1c)

if(is.null(mm)){} else { 
  df_new1c <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    select(., -contains(c("X.is.na.", "!is.na(")))  %>%
    dplyr::mutate(Source = "Same data source type"); rm(mm)
}


##+++++++++++
#Same data source type: 15-19
#Create output data:
#rara.function(docountry1d)
mm <- rara.function(docountry1d)

if(is.null(mm)){} else { 
  df_new1d <- with(mm, if ('ABR1519' %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    select(., -contains(c("X.is.na.", "!is.na(")))  %>%
    dplyr::mutate(Source = "Same data source type"); rm(mm)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##+++++++++++
#Data based on preponderant type of data source: 10-14
#Create output data:
rara.function(docountry2a)
mm <- rara.function(docountry2a)

if(is.null(mm)){} else { 
  df_new2a <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    dplyr::mutate(Source = "Preponderant type of data source",
                  AgeStart = 10); rm(mm)
}

##+++++++++++
#Data based on preponderant type of data source: 15-19
#Create output data:
#rara.function(docountry2b)
mm <- rara.function(docountry2b)

if(is.null(mm)){} else {
  #Create output data:
  df_new2b <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    dplyr::mutate(Source = "Preponderant type of data source",
                  AgeStart = 15); rm(mm)
}

# names(df_new1d)
# names(df_new2a)
# names(docountry3b)
#
# vv <- names(df_new1d) %nin% names(df_new2a)
# setdiff(names(df_new1d), names(df_new2a))

### Data selected and included in the Global SDG Indicators Database 2021:
df_new3b <- with(docountry3b, if ("!is.na(ABR1519)" %in% colnames(docountry3b)) {select(docountry3b, -contains("1519"))} else {})  %>%
  dplyr::mutate(Source = "Global SDG Indicators Database 2021",
                AgeStart = 15)

#Create emptydf to obtain columns to append to df_new3b and fill in the missing column names.
#https://stackoverflow.com/questions/4269012/create-empty-dataframe-in-r-with-same-columns
emptydf <- df_new1a[0,]
str(df_new1a)
df_new3b <- df_new3b %>%
  rbind.fill(., emptydf); rm(emptydf)

# Append output data ------------------------------------------------------
## https://stackoverflow.com/questions/25317362/rbind-multiple-dataframes-within-a-function

docountry_outputdata <- do.call(rbind, return(list(df_new1a, df_new1b, df_new1c, df_new1d, df_new2a, df_new2b, df_new3b))); #rm(outputlist)
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++


} , silent = TRUE)

####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####++++++++++++++  End of countrydata.funtion +++++++++++++++++++++++++++++
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Output selected data as list (from functions) ---------------------------
compiledatafromlist.function <- function(x) tryCatch(countrydata.function(x), error = function(e) e)

####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

###++++++++++++++++ DataSourcesChart.fn Function ++++++++++++++++++++
DataSourcesChart.fn <- function(docountry, RefYear, ABR, DataCatalogShortName) {
  docountry %>%
    # ggplot( aes(x=RefYear, y=ABR, color = factor(RefYear), fill=ABR)) +
    ggplot( aes(x=RefYear, y=ABR)) +
      #geom_line(aes(linetype=DataCatalogShortName, color = DataCatalogShortName), alpha=0.9, size=1.1) +    
    #geom_point()  +
    geom_point(aes(color=DataCatalogShortName), size=3.5) + 
    geom_point(mapping = aes(color = DataCatalogShortName)) +  
          guides(color = "none", linetype = "none") +
    # geom_text_repel(aes(y = ABR, label = DataCatalogShortName, vjust = -1)) +
    #https://stackoverflow.com/questions/43023569/how-to-give-multiple-conditions-in-geom-text
    # geom_text(aes(label=ifelse((SDG2020Status=="Alternative" | SDG2020Status=="NEW"), ABRcat, "")), vjust = -0.7, hjust = -0.1) +
    theme_bw() +
    #https://stackoverflow.com/questions/10861773/remove-grid-background-color-and-top-and-right-borders-from-ggplot2
    theme(axis.line = element_line(colour = "black"),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          strip.background = element_rect(
            color="black", fill="white", size=0.8, linetype="solid")) +
    #https://stackoverflow.com/questions/30083002/ggplot-legend-list-is-larger-than-page  
     theme(
      legend.position="top", legend.direction = "vertical",
      panel.spacing = unit(0.1, "lines"), legend.margin=margin(),
      strip.text.x = element_text(size = 8), legend.title = element_blank(),
      legend.key.size=unit(1,"point")) +
    guides(colour=guide_legend(nrow=5)) + 
    #guides(colour=guide_legend(nrow=1)) + 
    # guides(fill = FALSE, size = FALSE) +
    guides(fill = "none", size = "none") +    
    ggtitle(paste(docountry$LocName[1],": All data points", sep = "")) +
    theme(plot.title = element_text(size=12, hjust = 0.5)) +
    theme(axis.title.x = element_text(size=10, hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    theme(axis.title.y = element_text(size=10, hjust = 0.5)) +
      xlab("Reference Year") +
      #ylab("ABR 10-14") +
    # geom_smooth(method=loess, se=TRUE, level = 0.95, span = 0.75,
    #             fullrange=TRUE, inherit.aes = TRUE) +
  scale_x_continuous(breaks = docountry$RefYear, guide = guide_axis(n.dodge = 2)) +
    #scale_y_continuous(breaks=seq(0, k, 20)) +
    #Set the maximum y-value to a factor of 1.25:
    coord_cartesian(ylim = c(0, max(docountry$ABR)*1.25), expand = TRUE)
}

###++++++++++++++++ End of DataSourcesChart.fn Function ++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




# Function to select data that fall within the CI of 95%: -----------------

# Find points over and under the confidence interval when using geom_stat / geom_smooth in ggplot2
# https://stackoverflow.com/questions/33082901/find-points-over-and-under-the-confidence-interval-when-using-geom-stat-geom-s

library(sp)

rara.function <- function(countrydata, RefYear, ABR, ggplot2, ggeffects, in_ci, ., Within_ci, DataCatalogShortName) {
  df <- countrydata
  # we have to build the plot first so ggplot can do the calculations
  ggplot(df,aes(RefYear, ABR)) +
    geom_point() +
    #geom_smooth(  se = TRUE, level = 0.80) -> gg +
    #geom_smooth(  se = TRUE, level = 0.90) -> gg #+
    geom_smooth(  se = TRUE, na.rm = TRUE, level = 0.95) -> gg
  
  # do the calculations
  gb <- ggplot_build(gg)
  
  # get the CI data
  p <- gb$data[[2]]
  
  rm(gg,gb)
  
  ####________________________________________________
  if(nrow(p) < 1){df <- mutate(df, in_ci="NA",	Within_ci="NA")} else {
    # make a polygon out of it
    
    poly <- data.frame(
      x=c(p$x[1],    p$x,    p$x[length(p$x)],    rev(p$x)), 
      y=c(p$ymax[1], p$ymin, p$ymax[length(p$x)], rev(p$ymax))
    )
    
    # test for original values in said polygon and add that to orig data
    # so we can color by it
    
    df$in_ci <- point.in.polygon(df$RefYear, df$ABR, poly$x, poly$y)
    
    rm(p, poly)
    
    # It needs a bit of tweaking (i.e that last point getting a 2 value) but I'm limited on time. NOTE that the point.in.polygon return values are:
    # 
    # 0: point is strictly exterior to pol
    # 1: point is strictly interior to pol
    # 2: point lies on the relative interior of an edge of pol
    # 3: point is a vertex of pol
    # so it should be easy to just change the code to TRUE/FALSE whether value is 0 or not.
    
    library(ggplot2)
    library(ggeffects)
    
    df <- mutate(df, Within_ci=ifelse(in_ci==0, "Outside CI", "Inside CI") ) %>% 
      subset(., !in_ci==0)
    
    levels(factor(df$in_ci))
    # names(df)
    
    # re-do the plot with the new data
    if(nrow(df) < 1){} else {
      ggplot(df,aes(RefYear, ABR)) +
        geom_point(aes(color=factor(in_ci), size = 0.5)) +
        #geom_smooth() +
        geom_line(linetype = "dashed", color="blue", alpha=0.9, size=1.2)  +
        #https://stackoverflow.com/questions/43023569/how-to-give-multiple-conditions-in-geom-text
        geom_text(aes(label=ifelse((Within_ci=="Outside CI"), DataSourceShortName, ""), vjust = -1))
    }
    #https://stackoverflow.com/questions/15994368/how-to-make-object-created-within-function-usable-outside
    return(df)
  }
  
  # rm(df)
  gc(); cat("\014"); clearhistory(); #rm_history_database
  
  ##++++ Extract data from a ggplot +++++##
  ## https://www.py4u.net/discuss/859372
  
}

####++++++++++++++  End of rara.funtion +++++++++++++++++++++++++++++
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Function to plot the charts for selected data ---------------------
ChartsSelectedData.function <- try(function(x){

############### Read a set of data files for a country into R ################################
############  (http://stats.idre.ucla.edu/r/codefragments/read_multiple/) ####################

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #   rm(list=ls(pattern="gg|^Count|^kk|^datas|^dd|^p|^docount|^mm|^p|^ap|^abr.c"))

  
    CountryID = x
  # CountryID <- "Afghanistan"
  # CountryID <- "Bangladesh"
    # CountryID <- "Albania" 
    # CountryID <- "Algeria"    
    # CountryID <- "American Samoa" 
    #  CountryID <- "Côte d'Ivoire"
    # CountryID <- "Antigua and Barbuda"
    # CountryID <- "Viet Nam"
    docountry1 <- subset(FinalSelectedData, LocName==CountryID)    
    docountry1a <- subset(FinalSelectedData, LocName==CountryID & AgeStart==10)
    docountry1b <- subset(FinalSelectedData, LocName==CountryID & AgeStart==15)

    ###____________________________________________________________________  
  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++__________+++++++++++++++++
#ABR 10-14:
# Change docountry1a$DataCatalogShortName to factor before changing its labels:
# How to convert a factor to an integer\numeric without a loss of information?
# https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
#str(docountry1a)
names(docountry1a)
docountry1a$DataCatalogShortName <- as.factor(as.character(docountry1a$DataCatalogShortName))
#str(docountry1a)
#+++++++++++__________++++++++++++++++
#___________________________________
#ABR 15-19:
docountry1b$DataCatalogShortName <- as.factor(as.character(docountry1b$DataCatalogShortName))

#___________________________________
#Combine levels for ABR 10-14 with levels for ABR 15-19 to have similar legend labels:
levels(docountry1a$DataCatalogShortName)
levels(docountry1b$DataCatalogShortName)
levels(docountry1a$DataCatalogShortName) %in% levels(docountry1b$DataCatalogShortName)
kk1a <- levels(docountry1a$DataCatalogShortName)
kk1b <- levels(docountry1b$DataCatalogShortName)

# kk1c <- levels(docountry1c$DataCatalogShortName)
# kk1d <- levels(docountry1d$DataCatalogShortName)

setdiff(kk1a, kk1b)
setdiff(kk1a, kk1b)
union(kk1a, kk1b)

# union(kk1c, kk1d)
#___________________________________
#+++++++++++__________+++++++++++++++++

#Plot chart:
if( nrow(docountry1a) < 1 ) {
  #skip
} else {
p1a <- DataSourcesChart.fn(docountry1a) +
    ggtitle(paste(docountry1a$LocName[1],": Selected data,\n ABR 10-14", sep = "")) +
    # ggtitle(paste("ABR 10-14", sep = "")) +  
  ylab("ABR 10-14") +
       scale_colour_discrete(drop=TRUE,
         limits = union(kk1a, kk1b))  +
  #Join all data points:
   #geom_line(data = docountry1a_select, colour="red", size=0.9)  +
   geom_line( linetype = "solid", color="blue", alpha=0.9, size=0.5) +
      #geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE) +
    labs(caption = paste("Data sources: ",docountry1a$Source[1], sep = "")) +
    theme(
      plot.caption = element_text(vjust = 1, face = "italic")
    ) 
        }

if( nrow(docountry1b) < 1 ) {
  #skip
} else {
p1b <- DataSourcesChart.fn(docountry1b) +
    ggtitle(paste(docountry1b$LocName[1],": Selected data,\n ABR 15-19", sep = "")) +
    # ggtitle(paste("ABR 15-19", sep = "")) +  
      ylab("ABR 15-19")  + 
# #https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin/6924503#6924503:
       # scale_colour_discrete(drop=TRUE,
       #   limits = levels(docountry1c$DataCatalogShortName))
       scale_colour_discrete(drop=TRUE,
         limits = union(kk1a, kk1b))  +
  #Join all data points:
   #geom_line(data = docountry1a_select, colour="red", size=0.9)  +
   geom_line( linetype = "solid", color="blue", alpha=0.9, size=0.5) +
      #geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)+
    labs(caption = paste("Data sources: ",docountry1b$Source[1], sep = "")) +
    theme(
      plot.caption = element_text(vjust = 1, face = "italic"),
    ) 
}

#___________________________________
#_________________________________________________________________
if( exists('p1a') & exists('p1b') ) {
  print(p1a + p1b)
} else {
    #skip
  }

} , silent = TRUE)

####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####++++++++++++++  End of ChartsSelectedData.function +++++++++++++++++++++++++++++
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Plot chart for Bangladesh example:

DataSourcesChart_Bangladesh.fn <- function(docountry, RefYear, ABR, DataCatalogShortName) {
  docountry %>%
    # docountry1c_Bangladesh %>%
    # ggplot( aes(x=RefYear, y=ABR, color = factor(RefYear), fill=ABR)) +
    ggplot( aes(x=RefYear, y=ABR)) +
      geom_point(aes(linetype=DataCatalogShortName, color = DataCatalogShortName), alpha=0.9, size=1.1) +    
    #geom_point()  +
    geom_point(aes(color=DataCatalogShortName), size=3.5) + 
    geom_point(mapping = aes(color = DataCatalogShortName)) +  
          guides(color = "none", linetype = "none") +
    # geom_text_repel(aes(y = ABR, label = DataCatalogShortName, vjust = -1)) +
    #https://stackoverflow.com/questions/43023569/how-to-give-multiple-conditions-in-geom-text
    # geom_text(aes(label=ifelse((SDG2020Status=="Alternative" | SDG2020Status=="NEW"), ABRcat, "")), vjust = -0.7, hjust = -0.1) +
    theme_bw() +
    #https://stackoverflow.com/questions/10861773/remove-grid-background-color-and-top-and-right-borders-from-ggplot2
    theme(axis.line = element_line(colour = "black"),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          strip.background = element_rect(
            color="black", fill="white", size=0.8, linetype="solid")) +
    #https://stackoverflow.com/questions/30083002/ggplot-legend-list-is-larger-than-page  
     theme(
      legend.position="top", legend.direction = "vertical",
      panel.spacing = unit(0.1, "lines"), legend.margin=margin(),
      strip.text.x = element_text(size = 8), legend.title = element_blank(),
      legend.key.size=unit(1,"point")) +
    guides(colour=guide_legend(nrow=5)) + 
    #guides(colour=guide_legend(nrow=1)) + 
    # guides(fill = FALSE, size = FALSE) +
    guides(fill = "none", size = "none") +    
    ggtitle(paste(docountry$LocName[1],": All data points", sep = "")) +
    theme(plot.title = element_text(size=12, hjust = 0.5)) +
    theme(axis.title.x = element_text(size=10, hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    theme(axis.title.y = element_text(size=10, hjust = 0.5)) +
      xlab("Reference Year") +
      #ylab("ABR 10-14") +
    # geom_smooth(method=loess, se=TRUE, level = 0.95, span = 0.75,
    #             fullrange=TRUE, inherit.aes = TRUE) +
  scale_x_continuous(breaks = docountry$RefYear, guide = guide_axis(n.dodge = 2)) +
    #scale_y_continuous(breaks=seq(0, k, 20)) +
    #Set the maximum y-value to a factor of 1.25:
    coord_cartesian(ylim = c(0, max(docountry$ABR)*1.25), expand = TRUE)
}

###++++++++++++++++ End of DataSourcesChart_Bangladesh.fn Function ++++++++++++++++++++

