####____________________________________________________________________________
# title: "ABR 10-14 and ABR 15-19 datasets from DemoData"
# purpose: Load ABR data reported for SDG2021 and merge them with the data from DemoData
# purpose: Plot charts for visual check of the all the data currently available
# output: charts in fig directory
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

# Run file ("abr-selection-functions.R") that contains specifications of directories and functions:
source("abr-selection-functions.R")
options(scipen=999, digits=2)
####____________________________________________________________________________

# Directories (Working, Input, Utilities ----------------------------------
#Specified in "abr-selection-functions.R
here::here()

# Data sets ---------------------------------------------------------------

##Read in all the data downloaded using DemoData_ASFR.r:
# DT <- readRDS(file="DT.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse); library(nlme)
## All data sources for ABR 10-14 and ABR 15-19 from DemoData:
AllDataSourcesDemoData.data <- readRDS(file="Input Data/AllDataSourcesDemoData.data.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)

#____________
## Data with largest number of data sources for series of each country:
MaxDataSourceSeries.data <- readRDS(file="Input Data/MaxDataSourceSeries.data.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)

#____________
Locations <- as.data.frame(readRDS(file="Input Data/Locations.Rda"))
#____________

#++++++++++++++++++++++++++++++++++++++++++++++
#Read in all ABR data from which selection was made for 2021 (Indicator 3,7.2);
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


##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##+Import ABR data reported for SDG2021 and merge it with the data from DemoData:
####++++++++ Append data from df_1519 to ABR_SDG2021.data +++++++++

excel_sheets("Input Data/ABR_SDG2021_SelectedByChangeInABR.xlsx")
ABR_SDG2021.data <- read_excel("Input Data/ABR_SDG2021_SelectedByChangeInABR.xlsx", sheet = "ABR_SDG2021_FinalSelection") %>% 
  plyr::rename(., c("Country"="LocName", "ShortSource"="DataCatalogShortName"))

ABR_SDG2021.data <- as.data.frame(ABR_SDG2021.data) 
class(ABR_SDG2021.data)  

#Change values of levels (labels) for China:
ABR_SDG2021.data$DataCatalogShortName <- as.factor(as.character(ABR_SDG2021.data$DataCatalogShortName))
ifelse(ABR_SDG2021.data$LocName=="China", ABR_SDG2021.data$DataCatalogShortName <- mapvalues(ABR_SDG2021.data$DataCatalogShortName, from = c("2001 APCS ()", "2002 APCS ()","2003 APCS ()", "2004 APCS ()",
"2006 APCS ()", "2007 APCS ()", "2008 APCS ()", "2009 APCS ()", "2011 APCS ()", "2012 APCS ()", "2013 APCS ()", "2014 APCS ()", "2015 APCS ()"), to = c("2001 APCS", "2002 APCS", "2003 APCS", "2004 APCS", "2006 APCS", "2007 APCS", "2008 APCS", "2009 APCS", "2011 APCS", "2012 APCS", "2013 APCS", "2014 APCS", "2015 APCS")), ABR_SDG2021.data$DataCatalogShortName)

class(ABR_SDG2021.data)
saveRDS(ABR_SDG2021.data, file="Input Data/ABR_SDG2021.data.Rda")
# #dput(names(ABR_SDG2021.data))
#____________

###+++++++++++++++++++
## Select ABR 15-19 data from DemoData
#Subset ABR1519 data:
df_1519 <- AllDataSourcesDemoData.data %>%
  subset(AgeStart==15)

names(df_1519)

###+++++++++++++++++++
## Select ABR 10-14 data from DemoData
#Subset ABR1014 data:
df_1014 <- AllDataSourcesDemoData.data%>%
  subset(AgeStart==10)

names(df_1014)

#______________________________________________________
####++++++++ Append data from df_1519 to ABR_AllDatapoints2021.data +++++++++
#	Combine data.frames by row, filling in missing columns.
#https://stackoverflow.com/questions/4269012/create-empty-dataframe-in-r-with-same-columns
#emptydf <- MaxDataSourceSeries.data[0,]
names(df_1519)
names(ABR_AllDatapoints2021.data)
df_1519 <- ABR_AllDatapoints2021.data %>%
  rbind.fill(., df_1519)

#______________________________________________________
library(dplyr)
#Remove duplicate data that were not reported in 2021 SDG reporting:
# df_1519 <- df_1519 %>%
#   group_by(LocID, RefYear, round(df_1519$ABR,digits=0)) %>%
#   filter(!(is.na(DataType) & n() > 1)) %>% # filter(!is.na(DataType) | n() == 1))
#   ungroup()

####___________________________________________________________________________________
#'* All ABR data available in 2022: *
ABR_AllDatapoints2022.data <- df_1519 %>%
  rbind.fill(., df_1014)

rm(df_1014, df_1519)
####___________________________________________________________________________________

#Change values of levels (labels) for China in ABR_AllDatapoints2022.data:
ABR_AllDatapoints2022.data$DataCatalogShortName <- as.factor(as.character(ABR_AllDatapoints2022.data$DataCatalogShortName))
ifelse(ABR_AllDatapoints2022.data$LocName=="China", ABR_AllDatapoints2022.data$DataCatalogShortName <- mapvalues(ABR_AllDatapoints2022.data$DataCatalogShortName, from = c("2001 APCS ()", "2002 APCS ()","2003 APCS ()", "2004 APCS ()",
"2006 APCS ()", "2007 APCS ()", "2008 APCS ()", "2009 APCS ()", "2011 APCS ()", "2012 APCS ()", "2013 APCS ()", "2014 APCS ()", "2015 APCS ()"), to = c("2001 APCS", "2002 APCS", "2003 APCS", "2004 APCS", "2006 APCS", "2007 APCS", "2008 APCS", "2009 APCS", "2011 APCS", "2012 APCS", "2013 APCS", "2014 APCS", "2015 APCS")), ABR_AllDatapoints2022.data$DataCatalogShortName)


####___ Replace specified values in a vector or factor with new values. ## 
ifelse(ABR_AllDatapoints2022.data$LocName=="Bangladesh" & 
   ABR_AllDatapoints2022.data$DataCatalogShortName=="2019 SVRS",
  ABR_AllDatapoints2022.data$DataCatalogShortName <- plyr::mapvalues(ABR_AllDatapoints2022.data$DataCatalogShortName,
              from=c("2019 SVRS"),
              to = c("SVRS")), ABR_AllDatapoints2022.data$DataCatalogShortName)

#______________________________________________________
##+++++++++++++++++++++++++++++++
#Replace missing data on DataProcessType in ABR_AllDatapoints2022.data:
library(sjmisc);library(dplyr); library(stringr)
names(ABR_AllDatapoints2022.data)
ABR_AllDatapoints2022.data$Source <- as.factor(as.character(ABR_AllDatapoints2022.data$Source))
levels(ABR_AllDatapoints2022.data$Source)

ABR_AllDatapoints2022.data <- ABR_AllDatapoints2022.data %>% mutate(DataProcessType = case_when(
    is.na(DataProcessType) &
    str_detect(Source, "Census") ~ "Census",
  
    is.na(DataProcessType) &
    (str_detect(Source, "Reg") | str_detect(DataCatalogShortName, "Reg") | 
      str_detect(DataCatalogShortName, "Women and men of the Kyrgyz") |
       str_detect(DataCatalogShortName, "Vital Data Observatory")) ~ "Registration",
  is.na(DataProcessType)  &
     (str_detect(Source, "Survey") | str_detect(Source, "Panel"))~ "Survey",

  is.na(DataProcessType) &
    str_detect(Source, "Round") ~ "Survey",

 is.na(DataProcessType) &
    (str_detect(Source, "Estimate") | str_detect(Source, "estimate")) ~ "Estimate",  
 
 is.na(DataProcessType) &
    str_detect(Source, "National Risk") ~ "Survey",  
 is.na(DataProcessType) &
    (str_detect(Source, "Enquête") | str_detect(Source, "Encuesta") 
     | str_detect(Source, "Etude") | str_detect(Source, "MICS")) ~ "Survey", 
 is.na(DataProcessType) &
    (str_detect(Source, "SRVS") | str_detect(Source, "SRS")) ~ "Registration", 
 is.na(DataProcessType) &
    (str_detect(Source, "Demographic") | 
       str_detect(Source, "Ministry of Planning") | 
       str_detect(Source, "Anuario Estadístico")) ~ "Estimate", 
                                         TRUE ~ DataProcessType)) 

#______________________________________________________
#Check if there are datapoints with missing values on DataProcessType:
gg <- ABR_AllDatapoints2022.data %>% filter(is.na(DataProcessType))
rm(gg)
#______________________________________________________

ABR_AllDatapoints2022.data$DataProcessType <- as.factor(as.character(ABR_AllDatapoints2022.data$DataProcessType))
levels(ABR_AllDatapoints2022.data$DataProcessType)

library("dplyr")
# Replace on selected column:
ABR_AllDatapoints2022.data <- ABR_AllDatapoints2022.data %>% 
  mutate(DataProcessType = str_replace(DataProcessType, "SRS", "Registration")) %>% 
  mutate(DataProcessType = str_replace(DataProcessType, "Register", "Registration")) %>% 
  mutate(DataProcessType = str_replace(DataProcessType, "Panel", "Survey")) %>%
  dplyr::arrange(LocName, AgeStart, RefYear)

saveRDS(ABR_AllDatapoints2022.data, file="Input Data/ABR_AllDatapoints2022.data.Rda")
#ABR_AllDatapoints2022.data <- readRDS(file="Input Data/ABR_AllDatapoints2022.data.Rda")
####___________________________________________________________________________________

#'*@SK https://stackoverflow.com/questions/38884860/colour-coding-comments-in-rstudio *

#'* Filter conditionally the ABR1519 data in a pipe in R: *
#'* https://stackoverflow.com/questions/65303959/filter-conditionally-in-a-pipe-in-r *

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

#Select country list for use in lapply function below:
#ABR_AllDatapoints2022.data <- readRDS(file="Input Data/ABR_AllDatapoints2022.data.Rda")
abr1014countrylist <- ABR_AllDatapoints2022.data %>%
  subset(AgeStart==10) %>%
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
  
  #rm(list=ls(pattern="Countryl"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Select countries which have ABR (15-19 years) but do not have ABR (10-14 years):
library(sjmisc) 
abr1519countrylist <- abr1519countrylist[(abr1519countrylist$LocID %nin% abr1014countrylist$LocID),]

###___________________________________________________________________________________
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##	Functions to calculate the desired data and print charts
# Function to plot the graph and output selected data ---------------------
# Apply function (abr1014.function) to plot and save the charts to a file:
###_______________________________________________________________________________        
gc(); cat("\014"); clearhistory(); #rm_history_database
###_______________________________________________________________________________

###_______________________________________________________________________________  
#'*Create a PDF file where the graphs will be printed for countries with both ABR1014 and ABR1519 data:*
  #'*https://stackoverflow.com/questions/8175912/load-multiple-packages-at-once*
  
  #'*https://stackoverflow.com/questions/15937131/print-to-pdf-file-using-grid-table-in-r-too-many-rows-to-fit-on-one-page *
graphics.off()
libraries("grDevices", "grid", "gridExtra")

# Create the output file:
Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
Sys.Date()

filename2 <- paste(fig.dir,"/ABR1019_for_SDG2022_",Date_today,".pdf", sep = "")
pdf(file = filename2, height = 11, width = 16,
    title = "Adolescent birth rates among girls aged 10 to 14 and 15 to 19 years")
rm(filename2)

#abr1014countrylist1 <- Countrylist %>% subset(., LocName=="Afghanistan" | LocName=="Uganda")
#Run the loop through countries
lapply(abr1014countrylist$LocName, abr1014.function)
dev.off()  # Turn off device driver (to flush output to PDF)

#	Create a PDF file where the graphs for ABR1519 will be printed for countries without corresponding ABR1014:
#'*https://stackoverflow.com/questions/15937131/print-to-pdf-file-using-grid-table-in-r-too-many-rows-to-fit-on-one-page *
graphics.off()
library(grDevices, grid, gridExtra)
# Create the output file:
Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
Sys.Date()

filename2 <- paste(fig.dir,"/ABR1519_for_SDG2022_",Date_today,".pdf", sep = "")
pdf(file = filename2, height = 11, width = 16,
    title = "Adolescent birth rates among girls aged 15 to 19 years")
rm(filename2)

#Run the loop through countries
lapply(abr1519countrylist$LocName, abr1014.function)
dev.off()  # Turn off device driver (to flush output to PDF)


# 
# 
# #  selected data as list (from functions) - ABR1014 ---------------------------
# #Compile data for ABR1014:
# 
# compiledatafromlist.function <- function(x) tryCatch(countrydata.function(x), error = function(e) e)
# df_list <- lapply(abr1014countrylist$LocName, compiledatafromlist.function) #%>%
#   #bind_rows(.)
# 
# df_list.backup <- df_list
# 
# require(dplyr)
# dff1014.data <- df_list[lengths(df_list) > 2] %>%
#   bind_rows(.); rm(df_list)

# library(purrr)
# df_list <- keep(df_list, ~ length(.x) > 2)
# vec_c(df_list$in_ci)

# #	Create a PDF file where the graphs for ABR1519 will be printed:
# #https://stackoverflow.com/questions/15937131/print-to-pdf-file-using-grid-table-in-r-too-many-rows-to-fit-on-one-page
# graphics.off()
# library(grDevices, grid, gridExtra)
# # Create the output file:
# Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
# Sys.Date()
# 
# filename2 <- paste(fig.dir,"/ABR1519_for_SDG2022_",Date_today,".pdf", sep = "")
# pdf(file = filename2, height = 11, width = 16,
#     title = "Adolescent birth rates among girls aged 15 to 19 years")
# rm(filename2)
# 
# #Run the loop through countries
# lapply(abr1519countrylist$LocName, abr1014.function)
# dev.off()  # Turn off device driver (to flush output to PDF)
# 
# # Compile selected from list (from functions) - ABR1519 ---------------------------
# #Use functions (countrydata.function and compiledatafromlist.function) to extract data from lists
# 
# #Compile data for ABR1519:
# compiledatafromlist.function <- function(x) tryCatch(countrydata.function(x), error = function(e) e)
# df_list <- lapply(abr1519countrylist$LocName, compiledatafromlist.function) #%>%
# #bind_rows(.) 
# 
# require(dplyr)
# # dff.data <- df_list[lengths(df_list) > 2] %>%
# #   dplyr::bind_rows(.) 
# dff1519.data <- df_list[lengths(df_list) > 3] %>%
#   bind_rows(.); rm(df_list) 
# 
# # ####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #Bind the data for ABR (10-14 and 15-19 years) and rearrange order of columns:
# library(dplyr)
# #ABR data to be used for printing final charts:
# names(dff1014.data)
# names(dff1519.data)
# 
# setdiff(colnames(dff1014.data),colnames(dff1519.data))
# 
# dff1019all.data <- rbind(dff1014.data, dff1519.data) %>%
#   dplyr::arrange(LocName, AgeStart, RefYear)
# 
# ###++++++++++++++++++++++++++++++++++++++++++++++++
# #Filter out datapoints with RecallLag > 7 years:
# names(dff1019all.data)
# dff1019all.data <- dff1019all.data %>%
#   filter(is.na(RecallLag) | RecallLag < 8)
# 
# 
# library(dplyr)
# #Remove duplicate data that were not reported in 2021 SDG reporting:
# dff1019all.data <- dff1019all.data %>%
#   # Bangladesh.data <- dff1019.data %>%  
#   #   subset(., LocName=="Afghanistan") %>%
#   group_by(LocID, RefYear, AgeStart) %>%
#   filter(!(is.na(SDG2021Status) & n() > 1 & (abs(ABR - lag(ABR)) < 2))) %>%
#   filter(!(is.na(SDG2021Status) & n() > 1)) %>%
#   ungroup()
# ###++++++++++++++++++++++++++++++++++++++++++++++++
# 
# #ABR without the false data for ABR1014:
# dff1019.data <- dff1019all.data %>%
#   filter(!(AgeStart==10 & ABR > 90)) %>%
#   dplyr::arrange(LocName, AgeStart, RefYear)
# 
# 
# rm(dff1014.data, dff1519.data, abr1014countrylist, abr1519countrylist)
# #++++++++++++++++++++++++++++++++++++++++++++++
# 
# #++++++++++++++++++++++++++++++++++++++++++++++
# ##Save the returned data using saveRDS:
# saveRDS(dff1019all.data, file="Input Data/ABR_SDG2022_SelectedDataForChecking.Rda")
# saveRDS(dff1019.data, file="Input Data/ABR_SDG2022_DataForComputingAnnualChange.Rda")
# ##Then read it with:
# # dff1019all.data <- readRDS(file="Input Data/ABR_SDG2022_SelectedDataForChecking.Rda")
# ####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #++++++			END OF CHART PRINTING AND EXTRACTION OF DATA FROM LISTS ++++++++++++++
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# 
# #### OUTPUT DATA TO EXCEL FILE -------------------------------
# #https://stackoverflow.com/questions/54977747/r-error-in-as-vectorx-character-when-using-openxlsx-to-write-excel
#   library(xlsx) 
#   library(readxl)
#   library(openxlsx)
#   
# ####_______________________________________________________________________________
# # Create Excel output file:
# 
# Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
# Sys.Date()
# filename <- paste(output.dir,"/ABR_SDG2022_SelectedData.xlsx", sep = "")
# 
# #Move previously saved file to Previous Versions folder:
# if(file.exists(filename)) file.rename(filename, paste(output.dir, 
#                   "/ABR_SDG2022_SelectedData_", Date_today, ".xlsx", sep = ""))
# 
# file.create(filename)
# 
# # https://stackoverflow.com/questions/27713310/easy-way-to-export-multiple-data-frame-to-multiple-excel-worksheets
# 
# #library(readxl)
# library(colorspace)
# #http://dmcritchie.mvps.org/excel/colors.htm #Colour 
# #Colour of the worksheet tab. A valid colour (belonging to colours()) or a valid
# #hex colour beginning with "#"
# 
# library(openxlsx)
# 
# wb = openxlsx::createWorkbook(creator = "Stephen Kisambira",
#                               title = "ABR for SDG Reporting: Checking and Final Data",
#                               subject = "SDG Reporting, Indicator 3.7.2",
#                               category = "SDG data update")
# 
# #Add worksheet:
# #https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
#       openxlsx::addWorksheet(wb, "ABR_AllDatapoints2022", tabColour = "yellow",
#                        footer = c("&[Path]&[File]", NA, "&[Tab]"))
#       
#       openxlsx::writeDataTable(wb, "ABR_AllDatapoints2022", ABR_AllDatapoints2022.data,
#                startRow = 1, startCol = 1, tableStyle = "TableStyleLight8")   
#       
#       openxlsx::addWorksheet(wb, "SelectedDataForChecking", tabColour = "blue",
#                        footer = c("&[Path]&[File]", NA, "&[Tab]"))
#       
#       openxlsx::writeDataTable(wb, "SelectedDataForChecking", dff1019.data,
#                startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")     
# 
# #________________________
# #https://rdrr.io/cran/openxlsx/man/setColWidths.html
# setColWidths(wb, sheet = 1, cols = 6, widths = "auto")
# #setColWidths(wb, sheet = 2, cols = 1:3, widths = "auto")
# 
# #Save workbook:
#         openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
# 
# #Check worksheets in filename:
#         excel_sheets(filename)
# 
# ###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# gc(); cat("\014"); clearhistory() #rm_history_database # Do a garbage collection
# #rm(list = ls()) #Remove all objects
# ###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# #START HERE on 2 December 2021
# 
# # 
# # 
# # # DRAW AND SAVE V-TREE FIGURES ---------------------------------------------------
# # 
# # ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # ##+++++++++++++++++++++++  Pre-selected Final Selected Data Set +++++++++++++++++++++++++
# # ####_________________________________________________________________________________
# # 
# # # Read in the most recent selceted data and remove outliers for selected countries --------
# # here::dr_here()
# # 
# # # Read in data to select the data source type with the largest number of data points:
# # #_______________________________________________________________________________________
# # 
# library(readxl)
# excel_sheets("Output Data/ABR_SDG2022_SelectedData.xlsx")
# 
# ABR_SDG2022_SelectedDataForChecking.data <- read_excel("Output Data/ABR_SDG2022_SelectedData.xlsx",sheet = "SelectedDataForChecking")
# 
# names(ABR_SDG2022_SelectedDataForChecking.data)
# 
# library(dtplyr)
# library(dplyr)
# library(data.table)
# ## Extract Unique Elements:
# abr_country.list <- ABR_SDG2022_SelectedDataForChecking.data %>%
#   subset(., select = c(LocID, LocName)) %>%
#   #dplyr::n_distinct(LocID, .keep_all = TRUE) %>%
#   unique(., by=c(LocID)) %>%
#   dplyr::arrange(LocName) #Total number of countries: 210
# 
# ##+++++++++++++++++++++++++++++++
# #Calculate the data source type with the largest number of data points:
# library(sjmisc);library(dplyr); library(stringr)
# names(ABR_SDG2022_SelectedDataForChecking.data)
# ABR_SDG2022_SelectedDataForChecking.data$DataProcessType <- as.factor(as.character(ABR_SDG2022_SelectedDataForChecking.data$DataProcessType))
# levels(ABR_SDG2022_SelectedDataForChecking.data$DataProcessType)
# 
# mm <- ABR_SDG2022_SelectedDataForChecking.data %>% # mutate(DataProcessType = case_when(
#   dplyr::group_by(LocID, LocName, AgeStart, DataProcessType) %>%
#   dplyr::summarize(Count = n())  %>%
#     ungroup()
# ####_____________________________________________________________________________
# # list rows of data that have missing values
# mm[!complete.cases(mm),]
# colnames(mm[!complete.cases(mm),])
# 
# DataDistbyAge <- mm
# # Distribution of data source: --------------------------------------------
# # vtree is a flexible tool for calculating and displaying variable trees
# #https://cran.r-project.org/web/packages/vtree/vignettes/vtree.html
# 
# str(mm)
# mm$AgeStart <- as.factor(as.character(mm$AgeStart))
# levels(mm$AgeStart)
# levels(mm$AgeStart)[levels(mm$AgeStart)=="10"] <- "10-14 years"
# levels(mm$AgeStart)[levels(mm$AgeStart)=="15"] <- "15-19 years"
# levels(mm$AgeStart)
# 
# library(vtree); library(dplyr)
# mm %>% dplyr::rename("Age group"= AgeStart) %>%
# vtree(., c("Age group","DataProcessType"))
# 
# #Distribution of data source by LocName and AgeStart (Bangladesh example):
# mm1 <- mm %>% subset(., LocName=="Bangladesh")
# 
# mm1 %>% dplyr::rename("Age group"= AgeStart) %>%
# vtree(., c("Age group", "DataProcessType", "Count"),
#       title = mm1$LocName[1], showpct=FALSE, showcount=FALSE,
#       palette=c(3,4)
#    #fillcolor = c(LocName = "PuBu",  AgeStart = "#e7d4e8", Source = "#99d8c9")
#    )
# 
# 
# rm(filename, mm, mm1)
# ####________________________________________________________________________________
# 
# 
# rm(list=ls(pattern="gg|^df|^merged|^bb|^Count|^kk|^datas|^dd|^p|^docount|^mm|^p|^ap|^abr.c")) ## remove temporary files that contains "temp"
# 
# ###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# gc(); cat("\014"); clearhistory(); #rm_history_database # Do a garbage collection
# ###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ####++++++++++++++  End of output to Excel +++++++++++++++++++++++++++++
# ####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ####________________________________________________________________________
# #help(":=")
# # help("%in%")
# 
# #####+++++++++++++++ END OF SELECTION OF FINAL DATA SET ++++++++++++++++++++++++++++++
# #####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# 
# ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ##____________________________ END OF FILE	________________________________________
# ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
