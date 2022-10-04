#---
# title: "ABR 10-15 datasets"
# purpose: to select ABR 1519 for SDG reporting
# author: "Stephen Kisambira"
# date: "03/02/2020"
# output: html_document, chart; excel file
#---

rm(list = ls()) #Remove all objects


# House-keeping functions -------------------------------------------------
####____________________________________________________________________________________________

clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}

####___________________________________________________________________________________________
####_________________________________________________________________________________________
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

####_________________________________________________________________________________________

# Libraries ---------------------------------------------------------------

.libPaths() 
.libPaths("/Library/Frameworks/R.framework/Versions/4.1/Resources/library") #for Mac
## First specify the packages of interest
my_packages = c("base64", "benchmarkme", "benchmarkme", "benchmarkmeData", "data.table", "demogsurv", "devtools", "diffobj", "dplyr", "e1071", "easypackages", "expss", "forcats", "foreign", "FSA", "gdata", "ggalt", "ggeffects", "ggstance", "ggplot2", "ggrepel", "ggtext", "gmodels", "graphics", "gsubfn", "gtools", "haven", "here", "Hmisc", "hrbrthemes", "httr", "jsonlite", "lattice", "magick", "magrittr", "maptools", "mgsub", "mosaic", "mosaicCore", "naniar", "openssl", "openxlsx", "patchwork", "phangorn", "plotrix", "plotrix","prettyR", "plyr", "png", "prettyGraphs", "prettyR", "prettyunits", "processx", "questionr", "RCurl", "rdhs", "remotes", "reshape","rlist", "rJava", "RMySQL", "rprojroot", "sessioninfo", "scales",  "sf", "sp", "stats", "stringr", "survey", "survival", "tables", "tictoc", "tidymodels", "tigerstats", "tidyr", "tidystats", "tidyverse", "timevis", "tweenr", "units", "utf8", "utils", "viridis", "weights", "XML")
#_________________________________________________________
??tidystats

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

# Update packages ---------------------------------------------------------
.libPaths()
####____________________________________________________________________________
tidyverse_conflicts(); tidyverse_update()
####____________________________________________________________________________
update.packages(ask=F)

# Java --------------------------------------------------------------------
#Check Java issues:
system('java -version') # How to tell if you are running 64 or 32 bit Java
library(rJava)

# Session info ------------------------------------------------------------

library(sessioninfo)
#sk ??sessionInfo
package_info("sessioninfo")
sessionInfo() #- Session information is invaluable because it captures all of the packages used in the current project.

###+++++++++++++++
gc(); cat("\014"); clearhistory(); ##rm_history_database
###+++++++++++++++
here::set_here(); here::dr_here(); here::here()
####____________________________________________________________________________

# Directories (Working, Input, Utilities ----------------------------------
####___________________________________________________________________________
gc(); cat("\014"); clearhistory(); ##rm_history_database

####_____________________________________________________________________________________
#Analysis directory:
analysis.dir = NULL ##<< Analysis directory. If NULL, folder "fig" in current working directory.
if(is.null(analysis.dir)){
  analysis.dir <- file.path(here::here(), "Analysis/")
  dir.create(analysis.dir, showWarnings = FALSE)
} else {}


####__________________________________________________________________________________________________________________
#Input directory:
input.dir = NULL ##<<  If NULL, create in current working directory.
if(is.null(input.dir)){
  input.dir <- file.path(here::here(), "Input Data/")
  dir.create(input.dir, showWarnings = FALSE)
} else {}

####__________________________________________________________________________________________________________________
################### Output directory:
#Input directory:
output.dir = NULL ##<<  If NULL, create in current working directory.
if(is.null(output.dir)){
  output.dir <- file.path(here::here(), "Output Data/")
  dir.create(output.dir, showWarnings = FALSE)
} else {}

####__________________________________________________________________________________________________________________
#MyUtils directory:
myutils.dir = NULL ##<< If NULL, create in current working directory.
if(is.null(myutils.dir)){
  myutils.dir <- file.path(here::here(), "MyUtils/")
  dir.create(myutils.dir, showWarnings = FALSE)
} else {}

####__________________________________________________________________________________________________________________

# Charts/figures directory:
fig.dir = NULL ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
if(is.null(fig.dir)){
  fig.dir <- file.path(here::here(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
} else {}


####___________________________________________________________________________________
gc(); cat("\014"); clearhistory(); #rm_history_database
####___________________________________________________________________________________


####__________________________________________________________________________________________
# Data sets ---------------------------------------------------------------
####____________ Load data _______________________________________
here::here()
####_______________________________________________________________________________
library(data.table)
library(magrittr)
Sys.getlocale()
library(readxl)

##Read in all the data downloaded using DemoData_ASFR.r:
# DT <- readRDS(file="DT.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse); library(nlme)
## All data sources for ABR 10-14 and ABR 15-19 from DemoData:
AllDataSourcesDemoData.data <- readRDS(file="AllDataSourcesDemoData.data.Rda") %>%
      arrange(LocName, RefYear)
#____________
## Data with largest number of data sources for series of each country:
MaxDataSourceSeries.data <- readRDS(file="MaxDataSourceSeries.data.Rda") %>%
      arrange(LocName, RefYear)
#____________
Locations <- readRDS(file="Locations.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++
#Read in data reported in 2021 for ABR (Indicator 3,7.2);
library(readxl)

#AlDatapoints file used for SDG 2021 reporting/selection:
excel_sheets("Input Data/ABR_AllDatapoints2021.xlsx")

ABR_AllDatapoints2021.data <- read_excel("Input Data/ABR_AllDatapoints2021.xlsx", sheet = "ABR - All Data Points") %>% 
      plyr::rename(., c("Country"="LocName", "ShortSource"="DataCatalogShortName")) %>%
        mutate(SDG2021Status = "Yes",
               AgeStart = 15) %>% 
  group_by(LocID, RefYear) %>% 
#https://stackoverflow.com/questions/46119308/conditionally-removing-duplicates  
  filter(!(SDG2020Status!="SDG2020" & n() > 1)) %>% # filter(!is.na(DataType) | n() == 1))  
      select(LocID, LocName, #SDG2020Status, 
             RefYear, ABR, AgeStart, DataType, DataCatalogShortName, Source, SDG2021Status)

#dput(names(ABR_AllDatapoints2021.data))

excel_sheets("Input Data/ABR_SDG2021_SelectedByChangeInABR.xlsx")
ABR_SDG2021.data <- read_excel("Input Data/ABR_SDG2021_SelectedByChangeInABR.xlsx", sheet = "ABR_SDG2021_FinalSelection") %>% 
      plyr::rename(., c("Country"="LocName", "ShortSource"="DataCatalogShortName"))

###+++++++++++++++++++
#Subset ABR1519 data:
df_1519 <- AllDataSourcesDemoData.data %>%
  subset(AgeStart==15)

names(df_1519)

###+++++++++++++++++++
#Subset ABR1014 data:
df_1014 <- AllDataSourcesDemoData.data%>%
  subset(AgeStart==10)

names(df_1014)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##+Import ABR data reported for SDG2021 and merge it with the data from DemoData:
####++++++++ Append data from df_1519 to ABR_AllDatapoints2021.data +++++++++
#	Combine data.frames by row, filling in missing columns.
#https://stackoverflow.com/questions/4269012/create-empty-dataframe-in-r-with-same-columns
#emptydf <- MaxDataSourceSeries.data[0,]
names(df_1519)
names(ABR_AllDatapoints2021.data)
df_1519 <- ABR_AllDatapoints2021.data %>%
  rbind.fill(., df_1519)

library(dplyr)
df_1519 <- df_1519 %>%
  group_by(LocID, RefYear) %>%
  filter(!(is.na(DataType) & n() > 1)) #%>% # filter(!is.na(DataType) | n() == 1))
  #subset(LocName=="Bangladesh")

ABR_AllDatapoints2022.data <- df_1519 %>%
  rbind.fill(., df_1014)

# ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #Update preponderant data sources
# 
# MaxDataSourceSeries_10 <- readRDS(file="MaxDataSourceSeries_10.Rda")
# MaxDataSourceSeries_15.data <- readRDS(file="MaxDataSourceSeries_15.Rda")
# 
# names(df_1519)
# names(MaxDataSourceSeries_15.data) #MDSS_15
# 
# MDSS_15 <- MaxDataSourceSeries_15.data %>%
#   rbind.fill(., df_1519)
# 
# 
# library(dplyr)
# MDSS_15 <- MDSS_15 %>% 
#   group_by(LocID, RefYear) %>% 
#   filter(!(is.na(DataType) & n() > 1)) #%>% # filter(!is.na(DataType) | n() == 1))
#   #subset(LocName=="Bangladesh")


#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
#saveRDS(DataCatalog, file="DataCatalog.data.Rda") #From DemoData_ASFR.R
##Then read it with:
DataCatalog.data <- readRDS(file="Input Data/DataCatalog.data.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++

#Subset: ReferenceYearMid >=2000 to cover the MDG/SDG reporting period
names(DataCatalog.data)
str(DataCatalog.data)

DataCatalog.data <- DataCatalog.data %>%
  #Compute RefYear:
  #mutate(RefYear = trunc(ReferenceYearStart + ((ReferenceYearEnd - ReferenceYearStart)/2))) %>%
  subset((ReferenceYearMid >= 2000 & isSubnational=="FALSE"))


#'*@SK https://stackoverflow.com/questions/38884860/colour-coding-comments-in-rstudio *
#'*@SK some colorfull comment *


####___________________________________________________________________________________
gc(); cat("\014"); clearhistory(); #rm_history_database
####___________________________________________________________________________________


#'* Filter conditionally the ABR1519 data in a pipe in R: *
#'* https://stackoverflow.com/questions/65303959/filter-conditionally-in-a-pipe-in-r *
#'
####___________________________________________________________________________________
gc(); cat("\014"); clearhistory(); #rm_history_database
####___________________________________________________________________________________


## Extract Unique Elements:
abr1519country.list <- ABR_AllDatapoints2022.data %>%
  subset(., select = c(LocID, LocName)) %>%
  #unique(., by=c("LoCID")) %>%
  dplyr::distinct(LocID, .keep_all = TRUE) %>%
  dplyr::arrange(LocName)


names(AllDataSourcesDemoData.data)
names(MaxDataSourceSeries.data)


# Libraries
library(ggrepel)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)


###___________________________________________________________________________________
##	Function to calculate the desired data (Begin here when errors occur and you correct them.

#  rm(list=ls(pattern="^df_new|^p1|^p2|^docountry"))
  
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to plot the graph and output selected data ---------------------
abr1014.function <- try(function(x){

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
    docountry <- subset(ABR_AllDatapoints2022.data, LocName==CountryID)
    # docountry1 <- subset(AllDataSourcesDemoData.data, LocName==CountryID)
    docountry1 <- subset(AllDataSourcesDemoData.data, LocName==CountryID)    
    
    docountry2 <- subset(MaxDataSourceSeries.data, LocName==CountryID)    
    #docountry3 <- subset(ABR_AllDatapoints2021.data, LocName==CountryID)    
    docountry3 <- subset(ABR_SDG2021.data, LocName==CountryID)
    # docountry3 <- subset(AllDataSourcesDemoData.data, LocName==CountryID) %>%
    #   subset(., !is.na(DataType_1519))
    
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
               filter(RefYear == max(RefYear)), nudge_x = -9.35, nudge_y = 18.65, size = 3) +
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
               filter(RefYear == max(RefYear)), nudge_x = -7.35, nudge_y = 16.65, size = 3) +  
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
  geom_line(data = docountry3b, colour= "blue", size=0.9) +
  # # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
  # #              filter(RefYear == max(RefYear)), nudge_x = -1.35, nudge_y = -7.65, size = 3) +
  # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -9.35, nudge_y = -30.65, size = 3) +
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












#Final selection of data for Bangladesh:

#Selectted data: 10-14
docountry1a_backup <- docountry1a
docountry1a <- rara.function(docountry1a)
kk1a <- drop.levels(docountry1a$DataCatalogShortName)

# Or specify the factor levels in the order you want
levels(kk1a)
#kk1a <- factor(kk1a, levels = c("2007 DHS","2011 DHS", "2014 DHS", "2017-2018 DHS","2019 MICS"))

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

#Plot chart:
if( nrow(docountry1a) < 1 ) {
  #skip
} else {
Bangladesh_p1a <- DataSourcesChart_Bangladesh.fn(docountry1a) +
    # ggtitle(paste("Figure 8."," Selected adolescent birth rates (10-14 years),\n",
    #               docountry1a$LocName[1], sep = "")) +
    ggtitle(paste("Selected adolescent birth rates (10-14 years),\n",
                  docountry1a$LocName[1], sep = "")) +
  ylab("ABR 10-14") +
      theme(plot.title = element_text(face = "italic")) +
       scale_colour_discrete(drop=TRUE,
  #https://stackoverflow.com/questions/38619402/how-to-reorder-the-items-in-a-legend
        breaks=c("2007 DHS","2011 DHS","2017-2018 DHS","2019 MICS"),
         limits = union(kk1a, kk1a))  +
  #Add plot from another source:
   #geom_line(data = docountry1a_select, colour="red", size=0.9)  +
   geom_line(data = docountry1a_select, linetype = "dashed", color="blue", alpha=0.9, size=0.8) +
  # geom_label(aes(label = DataCatalogShortName1), data = docountry1a_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -4.35, nudge_y = 12.65, size = 3) +
  geom_label(aes(label = DataCatalogShortName1), data = docountry1a_select %>%
               filter(RefYear == max(RefYear)), nudge_x = -8.35, nudge_y = 20.65, size = 3) #+
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      #geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
        }

docountry1a <- docountry1a_backup
Bangladesh_p1a


#___________________________________

# ABR 10-14 based on same data source type
# Select the ABR 10-14 data that have same data type as the ABR 15-19  data:
docountry1c <- subset(docountry1c, docountry1c$DataProcessType %in% docountry1d$DataProcessType)
# Select the ABR 10-14 data that have same data type as the ABR 15-19  data:
docountry1d <- subset(docountry1d, docountry1d$DataProcessType %in% docountry1c$DataProcessType)
# Select the ABR 10-14 data that would be selected based on the selected ABR 15-19:
docountry1c_select <- subset(docountry1c, docountry1c$RefYear %in% docountry3b$RefYear) %>%
  mutate(DataCatalogShortName1="Blue dashed line =\nselection based on\n reported ABR 15-19 in 2021")

#Combine levels for ABR 10-14 with levels for ABR 15-19 to have similar legend labels:
kk1c <- drop.levels(docountry1c$DataCatalogShortName)
kk1d <- drop.levels(docountry1d$DataCatalogShortName)


# Specify the factor levels in the order you want
union(kk1c, kk1d)
levels(kk1c)
kk1cd <- factor(kk1c, levels = c("2007 DHS","2011 DHS","2014 DHS","2017-2018 DHS","2019 MICS" ))

names(docountry1c)
#Plot chart:
if( nrow(docountry1c) < 1 ) {
  #skip
} else {
p1c <- DataSourcesChart.fn(docountry1c) +
    ggtitle(paste(docountry1c$LocName[1],": ABR (10-14 years),\n same type of data source\n as ABR 15-19 (years)", sep = "")) +
  ylab("ABR 10-14") +
       scale_colour_discrete(drop=TRUE,
  #https://stackoverflow.com/questions/38619402/how-to-reorder-the-items-in-a-legend
        breaks=c("2004 DHS", "2007 DHS","2011 DHS","2014 DHS","2017-2018 DHS","2019 MICS"),            
         limits = kk1c)  +         
  #Add plot from another source:
   #geom_line(data = docountry1c_select, colour="red", size=0.9) +
   geom_line(data = docountry1c_select, linetype = "dashed", color="blue", alpha=0.9, size=1.2) + 
  # geom_label(aes(label = DataCatalogShortName1), data = docountry1c_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -4.35, nudge_y = 12.65, size = 3) +
  # geom_label(aes(label = DataCatalogShortName1), data = docountry1c_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -5.35, nudge_y = 18.65, size = 3) +
  geom_label(aes(label = DataCatalogShortName1), data = docountry1c_select %>%
               filter(RefYear == max(RefYear)), nudge_x = -8.35, nudge_y = 20.65, size = 3) +  
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)+
    guides(colour=guide_legend(nrow=3))
      }

# #ABR 15-19:
# if( nrow(docountry1d) < 1 ) {
#   #skip
# } else {
# p1d <- DataSourcesChart.fn(docountry1d) +
#     ggtitle(paste(docountry1d$LocName[1],": Same data source type,\n ABR 15-19", sep = "")) +
#       ylab("ABR 15-19") + 
# # #https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin/6924503#6924503:
#        # scale_colour_discrete(drop=TRUE,
#        #   limits = levels(docountry1c$DataCatalogShortName))
#      scale_colour_discrete(drop=TRUE,
#   #https://stackoverflow.com/questions/38619402/how-to-reorder-the-items-in-a-legend
#         breaks=c("2004 DHS","2007 DHS","2011 DHS","2014 DHS","2017-2018 DHS","2019 MICS"),           
#          # limits = union(kk1c, kk1d))  +
#          limits = kk1cd)  +    
#   #Add plot from another source:
#    geom_line(data = docountry3b, colour= "blue", size=0.9) +
#   # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
#   #              filter(RefYear == max(RefYear)), nudge_x = -1.35, nudge_y = -7.65, size = 3) +
#   # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
#   #              filter(RefYear == max(RefYear)), nudge_x = -9.35, nudge_y = -30.65, size = 3) +  
#       #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
#       geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=FALSE, inherit.aes = TRUE)
# }
# #_________________________________________________________________
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Final selection of data for Bangladesh (same type of data source):

#Selectted data: 10-14
#Create  data:

#docountry1c_backup <- docountry1c
# docountry1c <- docountry1c_backup
docountry1c_Bangladesh <- rara.function(docountry1c)
#kk1c <- drop.levels(docountry1c_Bangladesh$DataCatalogShortName)
levels(kk1c)

#Plot chart:
if( nrow(docountry1c_Bangladesh) < 1 ) {
  #skip
} else {
Bangladesh_p1c <- DataSourcesChart_Bangladesh.fn(docountry1c_Bangladesh) +
    ggtitle(paste(docountry1c_Bangladesh$LocName[1],": Selected ABR (10-14 years),\n based on same type of data source\n as ABR (15-19 years)")) +
  ylab("ABR 10-14") +
      #theme(plot.title = element_text(face = "italic")) +
       scale_colour_discrete(drop=TRUE,
  #https://stackoverflow.com/questions/38619402/how-to-reorder-the-items-in-a-legend
        breaks=c("2004 DHS", "2007 DHS", "2011 DHS", "2014 DHS", "2017-2018 DHS", "2019 MICS"),
         limits = kk1c)  +
  #Add plot from another source:
   #geom_line(data = docountry1a_select, colour="red", size=0.9)  +
   geom_line(data = docountry1c_select, linetype = "dashed", color="blue", alpha=0.9, size=0.8) +
  geom_label(aes(label = DataCatalogShortName1), data = docountry1c_select %>%
               filter(RefYear == max(RefYear)), nudge_x = -8.35, nudge_y = 20.65, size = 3) +
    guides(colour=guide_legend(nrow=3)) #+
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      #geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
        }

Bangladesh_p1c



#+Print chart:
if( exists('p1c') & exists('Bangladesh_p1c') ) {
  print(p1c + Bangladesh_p1c)
} else {
    #skip
  }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
    ggtitle(paste(docountry2a$LocName[1],": ABR (10-14 years) from type of\n data source (",datasourcetype2a, "),\n with largest number of datapoints", sep = "")) +
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
               filter(RefYear == max(RefYear)), nudge_x = -8.35, nudge_y = 20.65, size = 3) + 
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE) 
}

p2a

#Final selection of data for Bangladesh (preponderant data source):

#Selectted data: 10-14
#Create  data:

#docountry1c_backup <- docountry1c
# docountry1c <- docountry1c_backup
docountry2a_Bangladesh <- rara.function(docountry2a)
kk2a <- drop.levels(docountry2a$DataCatalogShortName)
levels(kk2a)

#Plot chart:
if( nrow(docountry2a_Bangladesh) < 1 ) {
  #skip
} else {
Bangladesh_p2a <- DataSourcesChart_Bangladesh.fn(docountry2a_Bangladesh) +
    ggtitle(paste(docountry2a_Bangladesh$LocName[1],": Selected ABR (10-14 years),\n from type of data source (",datasourcetype2a,")\n with largest number of datapoints")) +
  ylab("ABR 10-14") +
      #theme(plot.title = element_text(face = "italic")) +
       scale_colour_discrete(drop=TRUE,
  #https://stackoverflow.com/questions/38619402/how-to-reorder-the-items-in-a-legend
        breaks=c("2004 DHS", "2007 DHS", "2011 DHS", "2014 DHS", "2017-2018 DHS", "2019 MICS"),
         limits = kk1c)  +
  #Add plot from another source:
   #geom_line(data = docountry1a_select, colour="red", size=0.9)  +
   geom_line(data = docountry2a_select, linetype = "dashed", color="blue", alpha=0.9, size=0.8) +
  geom_label(aes(label = DataCatalogShortName1), data = docountry2a_select %>%
               filter(RefYear == max(RefYear)), nudge_x = -8.35, nudge_y = 20.65, size = 3) +
    guides(colour=guide_legend(nrow=3)) #+
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      #geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
        }

Bangladesh_p2a

if( exists('p2a') & exists('Bangladesh_p2a') ) {
  print(p2a + Bangladesh_p2a)
} else {
    #skip
  }

# ######+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ######+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# if( nrow(docountry2b) < 1 ) {
#   #skip
# } else {
# tds2b <- unique(docountry2b$DataProcessType) 
# datasourcetype2b <- ifelse(length(tds2b) == 2, tolower(paste(tds2b[[1]]," and ", tds2b[[2]], sep = "")), tolower(paste(tds2b[[1]]))); rm(tds2b)
# 
# p2b <- DataSourcesChart.fn(docountry2b) +
#     ggtitle(paste(docountry2b$LocName[1],": Preponderant data\n source (",datasourcetype2b, "),\n ABR 15-19", sep = "")) +
#       ylab("ABR 15-19") + 
# #https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin/6924503#6924503:
#        scale_colour_discrete(drop=TRUE,
#          limits = levels(docountry2b$DataCatalogShortName))  +
#   #Add plot from another source:
#   geom_line(data = docountry3b, colour= "blue", size=0.9) +
#   # # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
#   # #              filter(RefYear == max(RefYear)), nudge_x = -1.35, nudge_y = -7.65, size = 3) +
#   # geom_label(aes(label = DataCatalogShortName), data = docountry3b %>%
#   #              filter(RefYear == max(RefYear)), nudge_x = -9.35, nudge_y = -30.65, size = 3) +
#       #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
#       geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
# }

#______________________________________
#_________________________________________________________________

# if( exists('p2a') & exists('p2b') ) {
#   print(p2a + p2b)
# } else {
#     #skip
#   }


#Final selection of data for Bangladesh (ABR 10-14 and ABR 15-19):

#Selected data: 10-14

docountry1a_Bangladesh <- rara.function(docountry1a)
kk1a <- drop.levels(docountry1a_Bangladesh$DataCatalogShortName)
levels(kk1a)

#Plot chart:
if( nrow(docountry1a_Bangladesh) < 1 ) {
  #skip
} else {
Bangladesh_p1a <- DataSourcesChart_Bangladesh.fn(docountry1a_Bangladesh) +
    ggtitle(paste("Selected ABR (10-14 years),", docountry1a_Bangladesh$LocName[1])) +
  ylab("ABR 10-14") +
      #theme(plot.title = element_text(face = "italic")) +
  #      scale_colour_discrete(drop=TRUE,
  # #https://stackoverflow.com/questions/38619402/how-to-reorder-the-items-in-a-legend
  #       breaks=c("2004 DHS", "2007 DHS", "2011 DHS", "2014 DHS", "2017-2018 DHS", "2019 MICS"),
  #        limits = kk1a)  +
  #Add plot from another source:
   #geom_line(data = docountry1a_select, colour="red", size=0.9)  +
   geom_line(data = docountry1a_Bangladesh, linetype = "dashed", color="blue", alpha=0.9, size=0.8) +
  # geom_label(aes(label = DataCatalogShortName1), data = docountry1a_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -8.35, nudge_y = 20.65, size = 3) +
    guides(colour=guide_legend(nrow=4)) #+
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      #geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
        }


Bangladesh_p1a

#++++++++++++ Selected ABR 15-19
docountry1b_Bangladesh <- rara.function(docountry1b)
kk1b <- drop.levels(docountry1b_Bangladesh$DataCatalogShortName)
levels(kk1b)

#Plot chart:
if( nrow(docountry1b_Bangladesh) < 1 ) {
  #skip
} else {
Bangladesh_p1b <- DataSourcesChart_Bangladesh.fn(docountry1b_Bangladesh) +
    ggtitle(paste("Selected ABR (15-19 years),",docountry1b_Bangladesh$LocName[1])) +
  ylab("ABR 15-19") +
      #theme(plot.title = element_text(face = "italic")) +
  #      scale_colour_discrete(drop=TRUE,
  # #https://stackoverflow.com/questions/38619402/how-to-reorder-the-items-in-a-legend
  #       breaks=c("2001 MHS and MMS","2004 DHS","2007 DHS","2009 MICS","2011 DHS","2014 DHS","2017-2018 DHS","2019 MICS","SVRS" ),
  #        limits = kk1b)  +
  #Add plot from another source:
   #geom_line(data = docountry1a_select, colour="red", size=0.9)  +
   geom_line(data = docountry1b_Bangladesh, linetype = "dashed", color="blue", alpha=0.9, size=0.8) +
  # geom_label(aes(label = DataCatalogShortName1), data = docountry1b_select %>%
  #              filter(RefYear == max(RefYear)), nudge_x = -8.35, nudge_y = 20.65, size = 3) +
    guides(colour=guide_legend(nrow=4)) #+
      #https://stackoverflow.com/questions/16134425/remove-line-from-geom-smooth-in-ggplot2
      #geom_smooth(method=loess, linetype=0, se=TRUE, fullrange=TRUE, inherit.aes = TRUE)
        }

Bangladesh_p1b


if( exists('Bangladesh_p1a') & exists('Bangladesh_p1b') ) {
  print(Bangladesh_p1a + Bangladesh_p1b)
} else {
    #skip
  }


# Distribution of data source: --------------------------------------------
# vtree is a flexible tool for calculating and displaying variable trees
#https://cran.r-project.org/web/packages/vtree/vignettes/vtree.html

#Distribution of data source by LocName and AgeStart (Bangladesh example):

#Calculate the data source type with the largest number of data points:
library(sjmisc);library(dplyr); library(magrittr); library(tidyverse)
# mm <- ABR_SDG2022_preSelectedDataForChecking.data %>%
#   dplyr::count(LocID, LocName, AgeStart, Source)
# mm <- ABR_SDG2022_preSelectedDataForChecking.data %>%
#   dplyr::group_by(LocID, LocName, AgeStart, Source) %>%
#   dplyr::summarize(Count = n())  %>%
#     ungroup()


# mm1 <- docountry1a
mm1 <- ABR_AllDatapoints2022.data %>% 
  subset(., LocName=="Bangladesh")



#Replace missing values on DataProcessType:
names(mm1)
dput(mm1$Source)
mm1 <- mm1 %>% mutate(DataProcessType = case_when(
    str_detect(Source, "Sample Vital Registration System") & is.na(DataProcessType) ~ 'SRVS',  
    str_detect(Source, "Survey") & is.na(DataProcessType) ~ 'Survey',  
    str_detect(Source, "SRVS")  & is.na(DataProcessType) ~ 'SRVS',
    str_detect(DataProcessType, "SRS") ~ 'SRVS',    
    TRUE ~ DataProcessType ))

levels(as.factor(mm1$DataProcessType))

# mm <- ABR_SDG2022_preSelectedDataForChecking.data %>%
#   dplyr::count(LocID, LocName, AgeStart, Source)
# mm <- ABR_SDG2022_preSelectedDataForChecking.data %>%
#   dplyr::group_by(LocID, LocName, AgeStart, Source) %>%
#   dplyr::summarize(Count = n())  %>%
#     ungroup()




mm1 <- mm1 %>%
  dplyr::group_by(LocID, LocName, AgeStart, DataProcessType) %>%
  dplyr::summarize(Count = n())  %>%
    ungroup()




####_____________________________________________________________________________


library(vtree)
names(mm1)
vtree(mm1, "DataProcessType")

mm1 <- ff

str(mm1)
mm1$AgeStart <- as.factor(as.character(mm1$AgeStart))
levels(mm1$AgeStart)
levels(mm1$AgeStart)[levels(mm1$AgeStart)=="10"] <- "10-14 years"
levels(mm1$AgeStart)[levels(mm1$AgeStart)=="15"] <- "15-19 years"
levels(mm1$AgeStart)



library(vtree); library(dplyr)
BangladeshVtree <- mm1 %>% dplyr::rename("Age group"= AgeStart,
                      "Type of data source"= DataProcessType,
                      "Count"= count1_max) %>%
vtree(., c("Age group", "Type of data source", "Count"),
      title = mm1$LocName[1], showpct=FALSE, showcount=FALSE, horiz=FALSE, 
      showrootcount = FALSE, varnamepointsize = 17,
      #splitwidth=Inf,
      palette=c(3,4)
   #fillcolor = c(LocName = "PuBu",  AgeStart = "#e7d4e8", Source = "#99d8c9")
   )

BangladeshVtree

rm(filename, mm, kk)
####________________________________________________________________________________


??vtree

