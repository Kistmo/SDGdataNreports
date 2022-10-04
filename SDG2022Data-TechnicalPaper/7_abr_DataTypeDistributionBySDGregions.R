
####____________________________________________________________________________
# title: "Data type distribution by SDG region and country, ABR (10-14 and 15-19 years)"
# purpose: Selecting ASFRs (ABRs) for SDG reporting
# author: "Stephen Kisambira"
# date: "16/08/2022"
# output: html_document, chart; excel file

# R citation: -------------------------------------------------------------
citation()
library(rstudioapi)
versionInfo()$citation

# House-keeping functions -------------------------------------------------
rm(list=ls()) #Remove all objects.
Sys.info()

source("abr-selection-functions.R")

####___________________________________________________________________________
#How to disable scientific notation?
# https://stackoverflow.com/questions/5352099/how-to-disable-scientific-notation
# https://stackoverflow.com/questions/53882326/setting-and-resetting-scipen-and-digits-in-r-options-with-package-settings
# You can turn it off with options(scipen = 999) and back on again with options(scipen = 0)

options(scipen=999, digits=2)

####______________________________________________________________________________
gc(); cat("\014"); clearhistory(); ##rm_history_database

# Libraries ---------------------------------------------------------------
####_________________________________________________________________________

.libPaths() 
## Uncomment line below and change library path if need be:
#.libPaths("/Library/Frameworks/R.framework/Versions/4.2/Resources/library") #for Mac

# # Directories (Working, Input, Utilities ----------------------------------
# ####_________________________________________________________________________________
# #Analysis directory:
# analysis.dir = NULL ##<< Analysis directory. If NULL, folder "fig" in current working directory.
# if(is.null(analysis.dir)){
#   analysis.dir <- file.path(here::here(), "Analysis//")
#   dir.create(analysis.dir, showWarnings = FALSE)
# } else {}
# 
# ####_______________________________________________________________________________
# #Input directory:
# input.dir = NULL ##<<  If NULL, create in current working directory.
# if(is.null(input.dir)){
#   input.dir <- file.path(here::here(), "Input Data/")
#   dir.create(input.dir, showWarnings = FALSE)
# } else {}
# 
# ####_____________________________________________________________________________
# ################### Output directory:
# #Output directory:
# output.dir = NULL ##<<  If NULL, create in current working directory.
# if(is.null(output.dir)){
#   output.dir <- file.path(here::here(), "Output Data/")
#   dir.create(output.dir, showWarnings = FALSE)
# } else {}
# 
# ####________________________________________________________________________________
# #MyUtils directory:
# myutils.dir = NULL ##<< If NULL, create in current working directory.
# if(is.null(myutils.dir)){
#   myutils.dir <- file.path(here::here(), "MyUtils/")
#   dir.create(myutils.dir, showWarnings = FALSE)
# } else {}
# 
# ####_______________________________________________________________________________
# 
# # Charts/figures directory:
# fig.dir = NULL ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
# if(is.null(fig.dir)){
#   fig.dir <- file.path(here::here(), "fig/")
#   dir.create(fig.dir, showWarnings = FALSE)
# } else {}
# 
# ####______________________________________________________________________________
# # Previous Versions directory:
# previousversion.dir = NULL ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
# if(is.null(previousversion.dir)){
#   previousversion.dir <- file.path(here::here(), "Previous Versions/")
#   dir.create(previousversion.dir, showWarnings = FALSE)
# } else {}
# 
# ####______________________________________________________________________________
# # Previous figure directory:
# previousfig.dir = NULL ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
# if(is.null(previousfig.dir)){
#   previousfig.dir <- file.path(paste(here::here(),"/fig", sep = ""), "Previous Versions/")
#   dir.create(previousfig.dir, showWarnings = FALSE)
# } else {}


##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##++++++++  Data type distribution by SDG region and country +++++++++++++++++++++++++
####_________________________________________________________________________________

# Read in data for plotting selected data points:
library(readxl)
excel_sheets("Output Data/ABR_SDG2022_Data.xlsx")

#set working df:
df <- read_xlsx("Output Data/ABR_SDG2022_Data.xlsx", 
                                sheet = "ABR_SDG2022_FinalSelection", 
#https://stackoverflow.com/questions/50947838/numerical-column-in-excel-gets-converted-as-logical
                                  guess_max = 10000)

# Merge SDG regions with Locations ----------------------------------------
#SDG Regions
Locations <- readRDS(file="Locations.Rda") %>%
  select(-c(LocName))

Locations$SDGregion <- as.factor(as.character(Locations$SDGregion))
levels(Locations$SDGregion)
names(Locations)

names(df)
library(dplyr)
df <- data.frame(left_join(df, Locations, by = c("LocID")))
df <- df %>%
  subset(., !is.na(SDGregion))

#++++++++++++++++++++++++++++++++++++++++++++++
library(gsubfn)
library(mgsub)

#Rename "DataProcessType" to "DataSourceType":
#https://stackoverflow.com/questions/35444292/r-rename-multiple-columns-by-extracting-part-of-the-name
names(df) <- sub("DataProcessType", "DataSourceType", names(df))

names(df)

#Data on ABR 10-14:
df1014 <-  df %>%
  subset(AgeStart == 10)

#Data on ABR 15-19:
df1519 <-  df %>%
  subset(AgeStart == 15)


# Count number of countries or areas: -------------------------------------
names(df1014)
## Extract Unique Elements with  one data point:
library(dplyr)
df1014country.list <- df1014 %>%
  subset(., select = c(LocName, SDGregion)) %>%
  #Remove countries with one data point;
  dplyr::count(LocName, SDGregion) %>% subset(., n < 2) %>%
  dplyr::arrange(LocName)

df1014country.list
#                         LocName                           SDGregion n
# 24                       Bhutan      Central Asia and Southern Asia 1
# 52  Dem. People's Rep. of Korea Eastern Asia and South-eastern Asia 1
# 94                  Isle of Man         Northern America and Europe 1
# 107                     Lebanon    Western Asia and Northern Africa 1
# 163       Saint Kitts and Nevis     Latin America and the Caribbean 1
# 167            Saint-Barthélemy     Latin America and the Caribbean 1

df1519country.list <- df1519 %>%
  subset(., select = c(LocName, SDGregion)) %>%
  #Remove countries with one data point;
  dplyr::count(LocName, SDGregion) %>% subset(., n < 2) %>%
  dplyr::arrange(LocName)

df1519country.list
#                       LocName                           SDGregion n
# 1 Dem. People's Rep. of Korea Eastern Asia and South-eastern Asia 1
# 2                 Isle of Man         Northern America and Europe 1
# 3   Saint Pierre and Miquelon         Northern America and Europe 1
# 4                  San Marino         Northern America and Europe 1


#Total number of countries for which data were selected for SDG reporting in 2022:
unique(df1014$LocName) #They are 215 with at least one data point
unique(df1519$LocName) #They are 225 with at least one data point
range(df1014$RefYear) #2000 - 2019
range(df1519$RefYear) #2000 - 2020

# Distribution of data points by SDG regions ---------------------------------
####_______________________________________________________________________________________
# str(df)
# vignette('programming')
library(dplyr)
names(df1014)

SDGdistribution1014 <- df1014 %>% 
  dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::count(SDGregion)  %>%
  plyr::rename(., c("SDGregion"="Region"))

SDGdistribution1014$`Age group` <-  "10-14 years"

SDGdistribution1014
#                                   SDGregion  n   Age group
# 1                 Australia and New Zealand  2 10-14 years
# 2            Central Asia and Southern Asia 14 10-14 years
# 3       Eastern Asia and South-eastern Asia 16 10-14 years
# 4           Latin America and the Caribbean 45 10-14 years
# 5               Northern America and Europe 49 10-14 years
# 6 Oceania (excl. Australia and New Zealand) 17 10-14 years
# 7                        Sub-Saharan Africa 49 10-14 years
# 8          Western Asia and Northern Africa 23 10-14 years

SDGdistribution1519 <- df1519 %>% 
  dplyr::distinct(., LocID, .keep_all = TRUE) %>%
      dplyr::count(SDGregion) %>%
  plyr::rename(., c("SDGregion"="Region"))

SDGdistribution1519$`Age group` <-  "15-19 years"

SDGdistribution1519

# SDGregion  n   Age group
# 1                 Australia and New Zealand  2 15-19 years
# 2            Central Asia and Southern Asia 14 15-19 years
# 3       Eastern Asia and South-eastern Asia 18 15-19 years
# 4           Latin America and the Caribbean 46 15-19 years
# 5               Northern America and Europe 50 15-19 years
# 6 Oceania (excl. Australia and New Zealand) 20 15-19 years
# 7                        Sub-Saharan Africa 50 15-19 years
# 8          Western Asia and Northern Africa 25 15-19 years


# Landlocked developing countries (LLDCs)
names(df1014)

LDCdistribution1014 <- df1014 %>% 
  subset(., !is.na(LDCsName)) %>%
  dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::count(LDCsName) %>%
  plyr::rename(., c("LDCsName"="Region"))

LDCdistribution1014$`Age group` <-  "10-14 years"

LDCdistribution1014 #45 countries

#15-19:
LDCdistribution1519 <- df1519 %>% 
  subset(., !is.na(LDCsName)) %>%
  dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::count(LDCsName) %>%
  plyr::rename(., c("LDCsName"="Region"))

LDCdistribution1519$`Age group` <-  "15-19 years"

LDCdistribution1519 #48 countries

# Landlocked developing countries (LLDCs)
#10-14:
names(df1014)
LLDCsdistribution1014 <- df1014 %>% 
  subset(., LLDCsSIDSName == "Landlocked developing countries (LLDCs)") %>%
  dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::count(LLDCsSIDSName) %>%
  plyr::rename(., c("LLDCsSIDSName"="Region"))

LLDCsdistribution1014$`Age group` <-  "10-14 years"

LLDCsdistribution1014 #32 countries

# Landlocked developing countries (LLDCs)
#15-19:
names(df1519)
LLDCsdistribution1519 <- df1519 %>% 
  subset(., LLDCsSIDSName == "Landlocked developing countries (LLDCs)") %>%
  dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::count(LLDCsSIDSName) %>%
  plyr::rename(., c("LLDCsSIDSName"="Region"))

LLDCsdistribution1519$`Age group` <-  "15-19 years"

LLDCsdistribution1519 # 32 countries

#Small island developing States (SIDS)
#10-14:
names(df1014)
SIDSdistribution1014 <- df1014 %>% 
  subset(., LLDCsSIDSName == "Small island developing States (SIDS)") %>%
  dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::count(LLDCsSIDSName) %>%
  plyr::rename(., c("LLDCsSIDSName"="Region"))

SIDSdistribution1014$`Age group` <-  "10-14 years"

SIDSdistribution1014 # 47 countries

#Small island developing States (SIDS)
#15-19:
names(df1519)
SIDSdistribution1519 <- df1519 %>% 
  subset(., LLDCsSIDSName == "Small island developing States (SIDS)") %>%
  dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::count(LLDCsSIDSName) %>%
  plyr::rename(., c("LLDCsSIDSName"="Region"))

SIDSdistribution1519$`Age group` <-  "15-19 years"

SIDSdistribution1519 # 51 countries

  # Append  data ------------------------------------------------------
  ## https://stackoverflow.com/questions/25317362/rbind-multiple-dataframes-within-a-function
  
  ABRregionaldistribution_data <- do.call(rbind.fill, 
        list(SDGdistribution1014, SDGdistribution1519,
             LDCdistribution1014, LDCdistribution1519,
             LLDCsdistribution1014, LLDCsdistribution1519,
             SIDSdistribution1014, SIDSdistribution1519))

 ABRregionaldistribution_data <-  ABRregionaldistribution_data %>%
   select(., Region, `Age group`, n) %>%
  plyr::rename(., c("n"="Number of countries")) 
  # #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Write output to Excel file --------------------------------
#https://stackoverflow.com/questions/54977747/r-error-in-as-vectorx-character-when-using-openxlsx-to-write-excel
library(openxlsx)
filename <- paste(output.dir,"ABR_SDG2022_Data.xlsx", sep = "")
wb <- openxlsx::loadWorkbook(filename)

options("openxlsx.borderStyle" = "thin")

#Remove and add worksheet:
#https://www.r-bloggers.com/2018/10/modifying-excel-files-using-openxlsx/
if (('ABR_SDG2022_RegionalDistr' %in% names(wb))) removeWorksheet(wb, 'ABR_SDG2022_RegionalDistr') 
 
openxlsx::addWorksheet(wb, "ABR_SDG2022_RegionalDistr", tabColour = "yellow",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))

#https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
openxlsx::writeDataTable(wb, "ABR_SDG2022_RegionalDistr", ABRregionaldistribution_data,
               startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")


####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Save workbook:
openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

rm(filename)
####________________________________________________________________________________


#____________________________________________________________________________________________
## objects()
rm(list=ls(pattern="^df|^hs|^mm|^kk|^ss|^ABR|^Fig|^wb|^wfd"))			## remove temporary files that contains "temp"

####___________________________________________________________________________________
gc(); cat("\014"); clearhistory() #rm_history_database
####___________________________________________________________________________________

####+++++++++++++++++++++   END OF FILE   ++++++++++++++++++++++++++++++++++++++++++++++++
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++













