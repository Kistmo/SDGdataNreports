####____________________________________________________________________________
# title: "ABR 10-14 datasets from DemoData"
# purpose: Select ASFRs (ABRs) for SDG reporting
# author: "Stephen Kisambira"
# date: "13/08/2022"
# output: html_document, chart; excel file

# R citation: -------------------------------------------------------------
citation()
library(rstudioapi)
versionInfo()$citation

# House-keeping functions -------------------------------------------------
rm(list=ls()) #Remove all objects.
Sys.info()

###+++++++++++++++
here::set_here(); here::dr_here(); here::here()
####____________________________________________________________________________

source("abr-selection-functions.R")

options(scipen=999, digits=2)

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


# Data sets ---------------------------------------------------------------=
here::here()
####_______________________________________________________________________________

##Read in all the data downloaded using DemoData_ASFR.r:
# DT <- readRDS(file="DT.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse); library(nlme)
## All data sources for ABR 10-14 and ABR 15-19 from DemoData:
AllDataSourcesDemoData.data <- readRDS(file="AllDataSourcesDemoData.data.Rda") %>%
      arrange(LocName, RefYear)
AllDataSourcesDemoData.data <- as.data.frame(AllDataSourcesDemoData.data) 

#____________
## Data with largest number of data sources for series of each country:
MaxDataSourceSeries.data <- readRDS(file="MaxDataSourceSeries.data.Rda") %>%
      arrange(LocName, RefYear)

MaxDataSourceSeries.data <- as.data.frame(MaxDataSourceSeries.data) 

#____________
Locations <- as.data.frame(readRDS(file="Locations.Rda"))
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

ABR_AllDatapoints2021.data <- as.data.frame(ABR_AllDatapoints2021.data) 
class(ABR_AllDatapoints2021.data)  
#dput(names(ABR_AllDatapoints2021.data))


excel_sheets("Input Data/ABR_SDG2021_SelectedByChangeInABR.xlsx")
ABR_SDG2021.data <- read_excel("Input Data/ABR_SDG2021_SelectedByChangeInABR.xlsx", sheet = "ABR_SDG2021_FinalSelection") %>% 
      plyr::rename(., c("Country"="LocName", "ShortSource"="DataCatalogShortName"))

ABR_SDG2021.data <- as.data.frame(ABR_SDG2021.data) 
class(ABR_AllDatapoints2021.data)  
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

#'* Filter conditionally the ABR1519 data in a pipe in R: *
#'* https://stackoverflow.com/questions/65303959/filter-conditionally-in-a-pipe-in-r *
#'

####___________________________________________________________________________________
gc(); cat("\014"); clearhistory(); #rm_history_database
####___________________________________________________________________________________

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
    # CountryID <- "Uganda"    
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
#_______________________			START PRINTING CHART    ____________________________

###_______________________________________________________________________________        
gc(); cat("\014"); clearhistory(); #rm_history_database
###_______________________________________________________________________________

graphics.off()
#	Create a PDF file where the graphs will be printed:
library(grDevices)

#	Create a PDF file where the graphs will be printed:
#https://stackoverflow.com/questions/15937131/print-to-pdf-file-using-grid-table-in-r-too-many-rows-to-fit-on-one-page
library(grDevices, grid, gridExtra)
# Create the output file:
Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
Sys.Date()

filename2 <- paste(fig.dir,"ABR1014_SDG2022_",Date_today,".pdf", sep = "")
pdf(file = filename2, height = 11, width = 16,
    title = "Adolescent birth rates among girls aged 10 to 14 and 15 to 19 years")
rm(filename2)

Countrylist <- ABR_AllDatapoints2022.data %>%
  subset(., select = c(LocID, LocName)) %>%
  #unique(., by=c("LoCID")) %>%
  dplyr::distinct(LocID, .keep_all = TRUE) %>%
  dplyr::arrange(LocName)

class(Countrylist)

#Countrylist1 <- Countrylist %>% subset(., LocName=="Afghanistan" | LocName=="Uganda")
#Run the loop through countries
lapply(Countrylist$LocName, abr1014.function)
#cat("\014"); gc(verbose = getOption("verbose"), reset = TRUE); clearhistory()

dev.off()  # Turn off device driver (to flush output to PDF)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++			END OF CHART PRINTING    +++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



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
    #docountry3 <- subset(AllDataSourcesDemoData.data, LocName==CountryID) %>%
    #  subset(., !is.na(DataType_1519))
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

#  data -------------------------------------------------------------

#All data sources: 10-14
#Create  data:
#rara.function(docountry1a)
mm <- rara.function(docountry1a)

if(is.null(mm)){} else {  
  #Create  data:
  df_new1a <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    select(., -contains(c("X.is.na.", "!is.na(")))  %>%
    dplyr::mutate(Source = "All data sources"); rm(mm)
}


#All data sources: 15-19
#Create  data:
#rara.function(docountry1b)
mm <- rara.function(docountry1b)

if(is.null(mm)){} else {  
  df_new1b <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    select(., -contains(c("X.is.na.", "!is.na(")))  %>%
    dplyr::mutate(Source = "All data sources"); rm(mm)
}

df_new1b <- select(df_new1b, colnames(df_new1a))

##+++++++++++
#Same data source type: 10-14
#Create  data:
#rara.function(docountry1c)
mm <- rara.function(docountry1c)

if(is.null(mm)){} else { 
  df_new1c <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    select(., -contains(c("X.is.na.", "!is.na(")))  %>%
    dplyr::mutate(Source = "Same data source type"); rm(mm)
}

#colnames(df_new1c)
# df_new1c <- select(df_new1c, colnames(df_new1a))

##+++++++++++
#Same data source type: 15-19
#Create  data:
#rara.function(docountry1d)
mm <- rara.function(docountry1d)

if(is.null(mm)){} else { 
  df_new1d <- with(mm, if ('ABR1519' %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    select(., -contains(c("X.is.na.", "!is.na(")))  %>%
    dplyr::mutate(Source = "Same data source type"); rm(mm)
}

# df_new1d <- select(df_new1d, colnames(df_new1a))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##+++++++++++
#Data based on preponderant type of data source: 10-14
#Create  data:
rara.function(docountry2a)
mm <- rara.function(docountry2a)

if(is.null(mm)){} else { 
  df_new2a <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    dplyr::mutate(Source = "Preponderant type of data source",
                  AgeStart = 10); rm(mm)
}

# df_new2a <- select(df_new2a, colnames(df_new1a))
##+++++++++++
#Data based on preponderant type of data source: 15-19
#Create  data:
#rara.function(docountry2b)
mm <- rara.function(docountry2b)

if(is.null(mm)){} else {
  #Create  data:
  df_new2b <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    dplyr::mutate(Source = "Preponderant type of data source",
                  AgeStart = 15); rm(mm)
}

# df_new2b <- select(df_new2b, colnames(df_new1a))
 
#++++++++++++++++++++++++++++++++++++++++++++++
### Data selected and included in the Global SDG Indicators Database 2021:
#rara.function(docountry2b)
mm <- rara.function(docountry3b)

if(is.null(mm)){} else {
  #Create  data:
  df_new3b <- with(mm, if ("ABR1519" %in% colnames(mm)) {select(mm, -contains("1519"))} else {
    select(mm, -contains("1014"))
  })  %>%
    dplyr::mutate(Source = "Global SDG Indicators Database 2021",
                  AgeStart = 15); rm(mm)
}

# df_new3b <- select(df_new3b, colnames(df_new1a))
 
# names(df_new1d)
# names(df_new2a)
# names(docountry3b)
#
# vv <- names(df_new1d) %nin% names(df_new2a)
# setdiff(names(df_new1d), names(df_new2a))

# ### Data selected and included in the Global SDG Indicators Database 2021:
# df_new3b <- with(docountry3b, if ("!is.na(ABR1519)" %in% colnames(docountry3b)) {select(docountry3b, -contains("1519"))} else {})  %>%
#   dplyr::mutate(Source = "Global SDG Indicators Database 2021",
#                 AgeStart = 15)

#Create emptydf to obtain columns to append to df_new3b and fill in the missing column names.
#https://stackoverflow.com/questions/4269012/create-empty-dataframe-in-r-with-same-columns
emptydf <- df_new1a[0,]
str(df_new1a)
df_new3b <- df_new3b %>%
  rbind.fill(., emptydf); rm(emptydf)

# Append  data ------------------------------------------------------
## https://stackoverflow.com/questions/25317362/rbind-multiple-dataframes-within-a-function

# docountry_data <- do.call(rbind, return(list(df_new1a, df_new1b, df_new1c, df_new1d, df_new2a, df_new2b, df_new3b))); #rm(list)

docountry_data <- do.call(rbind.fill, list(df_new1a, df_new1b, df_new1c, df_new1d, df_new2a, df_new2b, df_new3b)) #rm(list)
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++


} , silent = TRUE)

####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####++++++++++++++  End of countrydata.funtion +++++++++++++++++++++++++++++
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

###_______________________________________________________________________________
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@	 TO EXCEL	@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
####_______________________________________________________________________________

#  selected data as list (from functions) ---------------------------

# rm(df_list)
compiledatafromlist.function
compiledatafromlist.function <- function(x) tryCatch(countrydata.function(x), error = function(e) e)
df_list <- lapply(Locations$LocName, compiledatafromlist.function) #%>%
  #bind_rows(.) 

dff.data <- df_list[lengths(df_list) > 2] %>%
  bind_rows(.) 

#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
saveRDS(dff.data, file="ABR_SDG2022_SelectedData.Rda")
##Then read it with:
# dff.data <- readRDS(file="ABR_SDG2022_SelectedData.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++

#### Write  to Excel file --------------------------------
#https://stackoverflow.com/questions/54977747/r-error-in-as-vectorx-character-when-using-openxlsx-to-write-excel

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #I use the following pseudo-code to "append" new information to the same worksheet:
  
  #Resource: https://community.rstudio.com/t/append-function-of-xlsx-in-openxlsx/9351

  # library(readxl)
  # excel_sheets(filename)
  # remove.packages("rio")
  library(xlsx) 
  library(openxlsx)
  
####_______________________________________________________________________________
# Create the  file:

df <- dff.data # data:

Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
Sys.Date()
#filename1 <- paste(output.dir,"ABR_SDG2022_SelectedData_",Date_today,".xlsx", sep = "")
filename <- paste(output.dir,"ABR_SDG2022_SelectedData.xlsx", sep = "")

#Move previously saved file to Previous Versions folder:
if(file.exists(filename)) file.rename(filename, paste(previousversion.dir, 
                  "ABR_SDG2022_SelectedData_", Date_today, ".xlsx", sep = ""))

file.create(filename)

# https://stackoverflow.com/questions/27713310/easy-way-to-export-multiple-data-frame-to-multiple-excel-worksheets

library(readxl)
library(colorspace)
#http://dmcritchie.mvps.org/excel/colors.htm #Colour 
#Colour of the worksheet tab. A valid colour (belonging to colours()) or a valid
#hex colour beginning with "#"

library(openxlsx)

wb = openxlsx::createWorkbook(creator = "Stephen Kisambira",
                              title = "ABR for SDG2022 Reporting: Checking and Final Data",
                              subject = "SDG 2022 Reporting, Indicator 3.7.2",
                              category = "SDG data update")

#Add worksheet:
#https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
      openxlsx::addWorksheet(wb, "SelectedDataForChecking", tabColour = "blue",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))
      
      openxlsx::writeDataTable(wb, "SelectedDataForChecking", df,
               startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")

#________________________
#https://rdrr.io/cran/openxlsx/man/setColWidths.html
setColWidths(wb, sheet = 1, cols = 6, widths = "auto")
#setColWidths(wb, sheet = 2, cols = 1:3, widths = "auto")

#Save workbook:
        openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

#Check worksheets in filename:
        excel_sheets(filename)

# rm(filename, df)
# rm(list=ls(pattern="^kk|^df_|^mm|^merged|^bb"))	## remove temporary files
#   rm(list=ls(pattern="gg|^Count|^kk|^datas|^dd|^p|^docount|^mm|^pp|^ap|^abr.c"))## remove temporary files that contains "temp"

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
gc(); cat("\014"); clearhistory() #rm_history_database # Do a garbage collection
#rm(list = ls()) #Remove all objects
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 
# 
# # SELECT FINAL DATA SET ---------------------------------------------------
# 
# ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ##+++++++++++++++++++++++  Preselected Final Selected Data Set +++++++++++++++++++++++++
# ####_________________________________________________________________________________
# 
# # Read in the most recent selceted data and remove outliers for selected countries --------
# here::dr_here()
# 
# # Read in data to select the data source type with the largest number of data points:
# 
# #_______________________________________________________________________________________
# 
library(readxl)
excel_sheets("Output Data/ABR_SDG2022_SelectedData.xlsx")

ABR_SDG2022_preSelectedDataForChecking.data <- read_excel("Output Data/ABR_SDG2022_SelectedData.xlsx",sheet = "SelectedDataForChecking")

names(ABR_SDG2022_preSelectedDataForChecking.data)

# warnings()
library(dtplyr)
library(dplyr)
library(data.table)
## Extract Unique Elements:
abr_country.list <- ABR_SDG2022_preSelectedDataForChecking.data %>%
  subset(., select = c(LocID, LocName)) %>%
  #dplyr::n_distinct(LocID, .keep_all = TRUE) %>%
  unique(., by=c(LocID)) %>%
  dplyr::arrange(LocName) #Total number of countries: 210

##+++++++++++++++++++++++++++++++
#Calculate the data source type with the largest number of data points:
library(dplyr)
# mm <- ABR_SDG2022_preSelectedDataForChecking.data %>%
#   dplyr::count(LocID, LocName, AgeStart, Source)
mm <- ABR_SDG2022_preSelectedDataForChecking.data %>%
  dplyr::group_by(LocID, LocName, AgeStart, Source) %>%
  dplyr::summarize(Count = n())  %>%
    ungroup()

#ordered vector for Source:
mm$Source <- ordered(mm$Source, levels = c("All data sources",  "Same data source type", "Preponderant type of data source","Global SDG Indicators Database 2021"))

# # How to Find the Maximum Value by Group in R -----------------------------
# #find rows that contain max data points by country and age group:
# #https://www.statology.org/maximum-value-by-group-r/
# #https://www.infoworld.com/article/3573577/how-to-count-by-groups-in-r.html
# 
# #Select the data source type with the largest number of data points:
# library(dplyr)
# kk <- mm %>%
#   ungroup() %>%  # grouped arrange ignores groups 
#   arrange(LocID, LocName, AgeStart, desc(Count)) %>%
#     dplyr::group_by(LocID, LocName, AgeStart) %>%
#   slice(which.max(Count)); #rm(mm)
# 
# #Merge kk with ABR_SDG2022_preSelectedDataForChecking.data:
# bb <- ABR_SDG2022_preSelectedDataForChecking.data #Shorten the dataset name :)
# dput(names(bb))
# names(kk)
# 
# #help(options)
# options(scipen = 15, digits=2)
# merged1 <- left_join(kk, bb)
# 
# dput(names(merged1))
# 
# merged1 <- merged1 %>% 
#   select(LocID, LocName, AgeStart, RefYear, ABR, DataCatalogID, 
# DataCatalogShortName, DataSourceShortName, PeriodSpan, 
# PeriodGroupName, TimeLabel, RecallLag, DataProcessType, 
# DataReliabilityName, DataTypeName, DataSourceYear, ModelPatternName, 
# DataQuality, Source, in_ci, Within_ci)
# 
# 
# #### Write output to Excel file --------------------------------
# #https://stackoverflow.com/questions/54977747/r-error-in-as-vectorx-character-when-using-openxlsx-to-write-excel
# library(openxlsx)
# 
# # Load an existing .xlsx file
# #https://stackoverflow.com/questions/51039841/r-add-sheet-to-an-existing-excel-workbook
# filename
# 
# library(readxl)
# excel_sheets(filename)
# 
# wb <- openxlsx::loadWorkbook(filename)
# options("openxlsx.borderStyle" = "thin")
# 
# #Add worksheet (final selected data):
# openxlsx::addWorksheet(wb, "ABR_SDG2022_FinalSelection", tabColour = "green",
#                        footer = c("&[Path]&[File]", NA, "&[Tab]"))
# 
# #https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
# openxlsx::writeDataTable(wb, "ABR_SDG2022_FinalSelection", merged1,
#                startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
# 
# tidy_label_colours()
# #Add worksheet:
# #Number of datapoints by Source group:
# openxlsx::addWorksheet(wb, "DatapointsBySourceGroup", tabColour = "pink",
#                        footer = c("&[Path]&[File]", NA, "&[Tab]"))
# 
# #https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
# openxlsx::writeDataTable(wb, "DatapointsBySourceGroup", mm,
#                startRow = 1, startCol = 1, tableStyle = "TableStyleLight8")
# 
# #Add worksheet:
# #Source with largest datapoints:
# openxlsx::addWorksheet(wb, "SourceWithLargestDatapoints", tabColour = "yellow",
#                        footer = c("&[Path]&[File]", NA, "&[Tab]"))
# 
# #https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
# openxlsx::writeDataTable(wb, "SourceWithLargestDatapoints", kk,
#                startRow = 1, startCol = 1, tableStyle = "TableStyleLight8")
# 
# ####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# # Save previous output file:
# Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
# Sys.Date()
# 
# #Move previously saved file to Previous Versions folder:
# if(file.exists(filename)) file.rename(filename, paste(previousversion.dir, 
#                   "ABR_SDG2022_SelectedData_", Date_today, ".xlsx", sep = ""))
# 
# ####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# #Save workbook:
# openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
# 
# 
# 
# Distribution of data source: --------------------------------------------
# vtree is a flexible tool for calculating and displaying variable trees
#https://cran.r-project.org/web/packages/vtree/vignettes/vtree.html

#Distribution of data source by LocName and AgeStart (Bangladesh example):
mm1 <- mm %>% subset(., LocName=="Bangladesh")
library(vtree)
vtree(mm, "Source")

str(mm1)
mm1$AgeStart <- as.factor(as.character(mm1$AgeStart))
levels(mm1$AgeStart)
levels(mm1$AgeStart)[levels(mm1$AgeStart)=="10"] <- "10-14 years"
levels(mm1$AgeStart)[levels(mm1$AgeStart)=="15"] <- "15-19 years"
levels(mm1$AgeStart)

library(vtree); library(dplyr)
mm1 %>% dplyr::rename("Age group"= AgeStart) %>%
vtree(., c("Age group", "Source", "Count"),
      title = mm1$LocName[1], showpct=FALSE, showcount=FALSE,
      palette=c(3,4)
   #fillcolor = c(LocName = "PuBu",  AgeStart = "#e7d4e8", Source = "#99d8c9")
   )


rm(filename, mm, kk)
####________________________________________________________________________________


rm(list=ls(pattern="gg|^df|^merged|^bb|^Count|^kk|^datas|^dd|^p|^docount|^mm|^p|^ap|^abr.c")) ## remove temporary files that contains "temp"

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
gc(); cat("\014"); clearhistory(); #rm_history_database # Do a garbage collection
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####++++++++++++++  End of output to Excel +++++++++++++++++++++++++++++
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####________________________________________________________________________
#help(":=")
# help("%in%")

#####+++++++++++++++ END OF SELECTION OF FINAL DATA SET ++++++++++++++++++++++++++++++
#####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##____________________________ END OF FILE	________________________________________
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
