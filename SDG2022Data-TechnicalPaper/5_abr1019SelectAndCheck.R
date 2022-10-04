####____________________________________________________________________________
# title: "Select ABR1014 and ABR1519 from all data available in 2022"
# purpose: Select ABRs using locally weighted least squares regression model with confidence intervals of 95 per cent 
#output: Selected data in Excel format in Output directory
# author: "Stephen Kisambira"
# date: "13/08/2021"
# output: html_document, chart; excel file


# R citation: -------------------------------------------------------------
citation()
library(rstudioapi)
versionInfo()$citation

# House-keeping functions -------------------------------------------------
#rm(list=ls()) #Remove all objects.
Sys.info()

###+++++++++++++++
here::set_here(); here::dr_here(); here::here()
####____________________________________________________________________________

source("abr-selection-functions.R")

options(scipen=999, digits=2)

# Data sets ---------------------------------------------------------------=
here::here()
####_______________________________________________________________________________

##Read in all the data downloaded using DemoData_ASFR.r:
#++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse); library(nlme)
## All data sources for ABR 10-14 and ABR 15-19 from DemoData:
AllDataSourcesDemoData.data <- readRDS(file="Input Data/AllDataSourcesDemoData.data.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)
#AllDataSourcesDemoData.data <- as.data.frame(AllDataSourcesDemoData.data) 

#____________
## Data with largest number of data sources for series of each country:
MaxDataSourceSeries.data <- readRDS(file="Input Data/MaxDataSourceSeries.data.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)
#MaxDataSourceSeries.data <- as.data.frame(MaxDataSourceSeries.data) 
#++++++++++++++++++++++++++++++++++++++++++++++

# Read in ABR_SDG2021.data: -----------------------------------------------
ABR_SDG2021.data <- readRDS(file="Input Data/ABR_SDG2021.data.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++

# Read in ABR_AllDatapoints2022.data: -------------------------------------
ABR_AllDatapoints2022.data <- readRDS(file="Input Data/ABR_AllDatapoints2022.data.edited.Rda")


####___________________________________________________________________________________
gc(); cat("\014"); clearhistory(); #rm_history_database
####___________________________________________________________________________________


# Select country list for use in lapply function below: -------------------
# Libraries
library(ggrepel)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

abr1014countrylist <- ABR_AllDatapoints2022.data %>%
  subset(AgeStart==10 ) %>%
  dplyr::group_by(LocID, AgeStart) %>%
  ##Remove countries with one data point;
  dplyr::count(LocID, LocName) %>% subset(., n > 1) %>%
  ungroup() %>%
  dplyr::distinct(LocID, .keep_all = TRUE) %>%
  select(., LocID, LocName) %>%
  dplyr::arrange(LocName)

abr1519countrylist <- ABR_AllDatapoints2022.data %>%
  subset(AgeStart==15 ) %>%
  dplyr::group_by(LocID) %>%
  ##Remove countries with one data point;
  dplyr::count(LocID, LocName) %>% subset(., n > 1) %>%
  ungroup() %>%
  dplyr::distinct(LocID, .keep_all = TRUE) %>%
  select(., LocID, LocName) %>%
  dplyr::arrange(LocName)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Select countries which have ABR (15-19 years) but do not have ABR (10-14 years):
library(sjmisc) 
abr1519countrylist <- abr1519countrylist[(abr1519countrylist$LocID %nin% abr1014countrylist$LocID),]

###___________________________________________________________________________________
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Function to calculate the desired data and output selected data ---------------------
# Apply function (countrydata.function) to output the selected data to Excel file:
#Use functions (countrydata.function and compiledatafromlist.function) to extract data from lists
#Compile data for ABR1014 as list:

compiledatafromlist.function <- function(x) tryCatch(countrydata.function(x), error = function(e) e)
df_list <- lapply(abr1014countrylist$LocName, compiledatafromlist.function) #%>%
#bind_rows(.)

df_list.backup <- df_list

require(dplyr)
dff1014.data <- df_list[lengths(df_list) > 2] %>%
  bind_rows(.); rm(df_list)

###_______________________________________________________________________________

#Compile data for ABR1519 as list:
compiledatafromlist.function <- function(x) tryCatch(countrydata.function(x), error = function(e) e)
df_list <- lapply(abr1519countrylist$LocName, compiledatafromlist.function) #%>%
#bind_rows(.) 

require(dplyr)
# dff.data <- df_list[lengths(df_list) > 2] %>%
#   dplyr::bind_rows(.) 
dff1519.data <- df_list[lengths(df_list) > 2] %>%
  bind_rows(.); rm(df_list) 

# ####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Bind the data for ABR (10-14 and 15-19 years) and rearrange order of columns:
library(dplyr)
#ABR data to be used for printing final charts:
names(dff1014.data)
names(dff1519.data)

setdiff(colnames(dff1014.data),colnames(dff1519.data))

dff1019all.data <- rbind(dff1014.data, dff1519.data) %>%
  dplyr::arrange(LocName, AgeStart, RefYear)

###++++++++++++++++++++++++++++++++++++++++++++++++
# 
# #Remove duplicate data that were not reported in 2021 SDG reporting:
# library(dplyr)
# dff1019all.data <- dff1019all.data %>%
#   # Bangladesh.data <- dff1019.data %>%  
#   #   subset(., LocName=="Afghanistan") %>%
#   group_by(LocID, RefYear, AgeStart) %>%
#   filter(!(is.na(SDG2021Status) & n() > 1 & (abs(ABR - lag(ABR)) < 2))) %>%
#   filter(!(is.na(SDG2021Status) & n() > 1)) %>%
#   ungroup()
# ###++++++++++++++++++++++++++++++++++++++++++++++++

# Remove ABR1014 with false data for ABR1014:
dff1019.data <- dff1019all.data %>%
  filter(!(AgeStart==10 & ABR > 90)) %>%
  dplyr::arrange(LocName, AgeStart, RefYear)


# rm(dff1014.data, dff1519.data, abr1014countrylist, abr1519countrylist)
#++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
saveRDS(dff1019all.data, file="Input Data/ABR_SDG2022_SelectedDataForChecking.Rda")
saveRDS(dff1019.data, file="Input Data/ABR_SDG2022_DataForComputingAnnualChange.Rda")
##Then read it with:
# dff1019all.data <- readRDS(file="Input Data/ABR_SDG2022_SelectedDataForChecking.Rda")
# dff1019.data <- readRDS(file="Input Data/ABR_SDG2022_DataForComputingAnnualChange.Rda")
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++			END OF EXTRACTION OF DATA FROM LISTS ++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#### OUTPUT DATA TO EXCEL FILE -------------------------------
#https://stackoverflow.com/questions/54977747/r-error-in-as-vectorx-character-when-using-openxlsx-to-write-excel
library(xlsx) 
library(readxl)
library(openxlsx)

####_______________________________________________________________________________
# Load an existing .xlsx file
# https://stackoverflow.com/questions/27713310/easy-way-to-export-multiple-data-frame-to-multiple-excel-worksheets

#https://stackoverflow.com/questions/51039841/r-add-sheet-to-an-existing-excel-workbook
filename <- paste(output.dir,"/ABR_SDG2022_SelectedData.xlsx", sep = "")

library(readxl)
excel_sheets(filename)

wb <- openxlsx::loadWorkbook(filename)
options("openxlsx.borderStyle" = "thin")

library(colorspace)
#http://dmcritchie.mvps.org/excel/colors.htm #Colour 
#Colour of the worksheet tab. A valid colour (belonging to colours()) or a valid
#hex colour beginning with "#"


#Add worksheet:
#https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
# openxlsx::addWorksheet(wb, "ABR_AllDatapoints2022", tabColour = "yellow",
#                        footer = c("&[Path]&[File]", NA, "&[Tab]"))
# 
# openxlsx::writeDataTable(wb, "ABR_AllDatapoints2022", ABR_AllDatapoints2022.data,
#                          startRow = 1, startCol = 1, tableStyle = "TableStyleLight8")   

openxlsx::addWorksheet(wb, "SelectedDataForChecking2", tabColour = "blue",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))

openxlsx::writeDataTable(wb, "SelectedDataForChecking2", dff1019.data,
                         startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")     

#________________________
#https://rdrr.io/cran/openxlsx/man/setColWidths.html
setColWidths(wb, sheet = 1, cols = 6, widths = "auto")
#setColWidths(wb, sheet = 2, cols = 1:3, widths = "auto")

#Save workbook:
openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

#Check worksheets in filename:
excel_sheets(filename)

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
# #_______________________________________________________________________________________
# 
library(readxl)
excel_sheets("Output Data/ABR_SDG2022_SelectedData.xlsx")

ABR_SDG2022_SelectedDataForChecking.data <- read_excel("Output Data/ABR_SDG2022_SelectedData.xlsx",sheet = "SelectedDataForChecking2")

names(ABR_SDG2022_SelectedDataForChecking.data)

library(dtplyr)
library(dplyr)
library(data.table)
## Extract Unique Elements:
abr_country.list <- ABR_SDG2022_SelectedDataForChecking.data %>%
  subset(., select = c(LocID, LocName)) %>%
  #dplyr::n_distinct(LocID, .keep_all = TRUE) %>%
  unique(., by=c(LocID)) %>%
  dplyr::arrange(LocName) #Total number of countries: 210

##+++++++++++++++++++++++++++++++
#Calculate the data source type with the largest number of data points:
library(sjmisc);library(dplyr); library(stringr)
names(ABR_SDG2022_SelectedDataForChecking.data)
ABR_SDG2022_SelectedDataForChecking.data$DataProcessType <- as.factor(as.character(ABR_SDG2022_SelectedDataForChecking.data$DataProcessType))
levels(ABR_SDG2022_SelectedDataForChecking.data$DataProcessType)

mm <- ABR_SDG2022_SelectedDataForChecking.data %>% # mutate(DataProcessType = case_when(
  dplyr::group_by(LocID, LocName, AgeStart, DataProcessType) %>%
  dplyr::summarize(Count = n())  %>%
  ungroup()
####_____________________________________________________________________________
# list rows of data that have missing values
mm[!complete.cases(mm),]
colnames(mm[!complete.cases(mm),])

DataDistbyAge <- mm
# Distribution of data source: --------------------------------------------
# vtree is a flexible tool for calculating and displaying variable trees
#https://cran.r-project.org/web/packages/vtree/vignettes/vtree.html

str(mm)
mm$AgeStart <- as.factor(as.character(mm$AgeStart))
levels(mm$AgeStart)
levels(mm$AgeStart)[levels(mm$AgeStart)=="10"] <- "10-14 years"
levels(mm$AgeStart)[levels(mm$AgeStart)=="15"] <- "15-19 years"
levels(mm$AgeStart)

library(vtree); library(dplyr)
mm %>% dplyr::rename("Age group"= AgeStart) %>%
  vtree(., c("Age group","DataProcessType"))

#Distribution of data source by LocName and AgeStart (Bangladesh example):
mm1 <- mm %>% subset(., LocName=="Bangladesh")

mm1 %>% dplyr::rename("Age group"= AgeStart) %>%
  vtree(., c("Age group", "DataProcessType", "Count"),
        title = mm1$LocName[1], showpct=FALSE, showcount=FALSE,
        palette=c(3,4)
        #fillcolor = c(LocName = "PuBu",  AgeStart = "#e7d4e8", Source = "#99d8c9")
  )


rm(filename, mm, mm1)
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
