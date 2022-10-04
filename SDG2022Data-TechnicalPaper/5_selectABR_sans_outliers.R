####____________________________________________________________________________
# title: "ABR 10-14 and 15-19 selected from data without outliers"
# purpose: Select ASFRs (ABRs) for SDG reporting
# author: "Stephen Kisambira"
# date: "30/08/2020"
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
# DT <- readRDS(file="DT.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse); library(nlme)

#Read in ABR_AllDatapoints2022.data
ABR_AllDatapoints2022.data <- readRDS(file="Input Data/ABR_AllDatapoints2022.editeddata.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)

## All data sources for ABR 10-14 and ABR 15-19 from DemoData:
AllDataSourcesDemoData.data <- readRDS(file="Input Data/AllDataSourcesDemoData.data.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)
# AllDataSourcesDemoData.data <- as.data.frame(AllDataSourcesDemoData.data) 

#____________
## Data with largest number of data sources for series of each country:
MaxDataSourceSeries.data <- readRDS(file="Input Data/MaxDataSourceSeries.data.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)

# MaxDataSourceSeries.data <- as.data.frame(MaxDataSourceSeries.data) 
#All data points reported for SDG 2021
ABR_SDG2021.data <- readRDS(file="Input Data/ABR_SDG2021.data.Rda") %>%
  arrange(LocName, RefYear)

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

graphics.off()
#	Create a PDF file where the graphs will be printed:
library(grDevices)

#	Create a PDF file where the graphs will be printed:
#https://stackoverflow.com/questions/15937131/print-to-pdf-file-using-grid-table-in-r-too-many-rows-to-fit-on-one-page
library(grDevices, grid, gridExtra)
# Create the output file:
Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
Sys.Date()

filename2 <- paste(fig.dir,"ABR1019_for_SDG2022_2ndCheck_",Date_today,".pdf", sep = "")
pdf(file = filename2, height = 11, width = 16,
    title = "Adolescent birth rates among girls aged 10 to 14 and 15 to 19 years")
rm(filename2)

#abr1014countrylist1 <- Countrylist %>% subset(., LocName=="Afghanistan" | LocName=="Uganda")
#Run the loop through countries
lapply(abr1014countrylist$LocName, abr1014.function)
dev.off()  # Turn off device driver (to flush output to PDF)

#  selected data as list (from functions) - ABR1014 ---------------------------
#Compile data for ABR1014:

compiledatafromlist.function <- function(x) tryCatch(countrydata.function(x), error = function(e) e)
df_list <- lapply(abr1014countrylist$LocName, compiledatafromlist.function) #%>%
  #bind_rows(.)

df_list.backup <- df_list

require(dplyr)
dff1014.data <- df_list[lengths(df_list) > 3] %>%
  bind_rows(.); rm(df_list)

# library(purrr)
# df_list <- keep(df_list, ~ length(.x) > 2)
# vec_c(df_list$in_ci)

#	Create a PDF file where the graphs for ABR1519 will be printed:
#https://stackoverflow.com/questions/15937131/print-to-pdf-file-using-grid-table-in-r-too-many-rows-to-fit-on-one-page
graphics.off()
library(grDevices, grid, gridExtra)
# Create the output file:
Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
Sys.Date()

filename2 <- paste(fig.dir,"ABR1519_for_SDG2022_2ndCheck_",Date_today,".pdf", sep = "")
pdf(file = filename2, height = 11, width = 16,
    title = "Adolescent birth rates among girls aged 15 to 19 years")
rm(filename2)

#Run the loop through countries
lapply(abr1519countrylist$LocName, abr1014.function)
dev.off()  # Turn off device driver (to flush output to PDF)

# Compile selected from list (from functions) - ABR1519 ---------------------------
#Use functions (countrydata.function and compiledatafromlist.function) to extract data from lists

#Compile data for ABR1519:
compiledatafromlist.function <- function(x) tryCatch(countrydata.function(x), error = function(e) e)
df_list <- lapply(abr1519countrylist$LocName, compiledatafromlist.function) #%>%
#bind_rows(.) 

require(dplyr)
# dff.data <- df_list[lengths(df_list) > 2] %>%
#   dplyr::bind_rows(.) 
dff1519.data <- df_list[lengths(df_list) > 3] %>%
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

#ABR without the false data for ABR1014:
dff1019.data <- dff1019all.data %>%
  filter(!(AgeStart==10 & ABR > 90)) %>%
  dplyr::arrange(LocName, AgeStart, RefYear)

# library(dplyr)
# #Remove duplicate data that were not reported in 2021 SDG reporting:
# dff1019.data <- dff1019.data %>%
#   #subset(., LocName=="Afghanistan") %>%
#   group_by(LocID, RefYear, AgeStart) %>%
#   filter(!(is.na(SDG2021Status) & n() > 1 & (abs(ABR - lag(ABR)) < 2))) %>% 
#     filter(!(is.na(SDG2021Status) & n() > 1)) %>%
#   ungroup()

rm(dff1014.data, dff1519.data, abr1014countrylist, abr1519countrylist)
#++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
saveRDS(dff1019all.data, file="Input Data/ABR_SDG2022_SelectedDataFor2ndChecking.Rda")
saveRDS(dff1019.data, file="Input Data/ABR_SDG2022_DataForComputingAnnualChange.Rda")
##Then read it with:
# dff1019all.data <- readRDS(file="Input Data/ABR_SDG2022_SelectedDataFor2ndChecking.Rda")
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++			END OF CHART PRINTING AND EXTRACTION OF DATA FROM LISTS ++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#### OUTPUT DATA TO EXCEL FILE -------------------------------
#### Write output to Excel file --------------------------------
#https://stackoverflow.com/questions/54977747/r-error-in-as-vectorx-character-when-using-openxlsx-to-write-excel
  library(xlsx) 
  library(readxl)
  library(openxlsx)

  library(openxlsx)

####_______________________________________________________________________________
# Load Excel output file:
  filename <- paste(output.dir,"ABR_SDG2022_SelectedData.xlsx", sep = "")
  wb <- openxlsx::loadWorkbook(filename)

Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
Sys.Date()
filename <- paste(output.dir,"ABR_SDG2022_SelectedData.xlsx", sep = "")

#Move previously saved file to Previous Versions folder:
if(file.exists(filename)) file.rename(filename, paste(previousversion.dir, 
                  "ABR_SDG2022_SelectedData_", Date_today, ".xlsx", sep = ""))

# https://stackoverflow.com/questions/27713310/easy-way-to-export-multiple-data-frame-to-multiple-excel-worksheets

#library(readxl)
library(colorspace)
#http://dmcritchie.mvps.org/excel/colors.htm #Colour 
#Colour of the worksheet tab. A valid colour (belonging to colours()) or a valid
#hex colour beginning with "#"

#Add worksheet:
#https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
library(openxlsx)
      openxlsx::addWorksheet(wb, "SelectedDataFor2ndChecking", tabColour = "#00FFFF",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))

      openxlsx::writeDataTable(wb, "SelectedDataFor2ndChecking", dff1019all.data,
               startRow = 1, startCol = 1, tableStyle = "TableStyleLight8")
      
      openxlsx::addWorksheet(wb, "DataForComputingAnnualChange", tabColour = "lightblue",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))
      
      openxlsx::writeDataTable(wb, "DataForComputingAnnualChange", dff1019.data,
               startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")     

#________________________
#https://rdrr.io/cran/openxlsx/man/setColWidths.html
setColWidths(wb, sheet = 1, cols = 6, widths = "auto")
#setColWidths(wb, sheet = 2, cols = 1:3, widths = "auto")

#Save workbook:
        openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

#Check worksheets in filename:
        excel_sheets(filename)

#rm(filename,dff)
rm(list=ls(pattern="filen|^docount|^dff|^abr1"))
####________________________________________________________________________________        
        
#++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
cat("\014"); gc(verbose = getOption("verbose"), reset = TRUE); clearhistory()
###_________________________________________________________________________________
        
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###____________________________ END OF FILE	________________________________________
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###       
        
        

