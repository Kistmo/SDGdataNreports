####_________________________________________________________________________________
# title: "ABR 10-14 and 15-19 datasets from UNPD Portal"
# purpose: For SDG reporting
# Platform: Windows
# author: "Stephen Kisambira"
# date, revised: "16/09/2022"
# output: html_document, chart; excel file
####________________________________________________________________________________

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

# House-keeping functions -------------------------------------------------
#rm(list=ls()) #Remove all objects.
options("install.packages.compile.from.source" = "never")
Sys.info()


####______________________________________________________________________________
gc(); cat("\014"); clearhistory(); ##rm_history_database
####______________________________________________________________________________

# R wrapper to connect to DemoData and download/filter ASFR data ----------

## Examples
## https://timriffe.github.io/DDSQLtools/articles/Downloading-UNPD-data-into-R.html

## get_recorddata(): Download data from UNPD portal
## https://timriffe.github.io/DDSQLtools/reference/get_recorddata.html
## https://timriffe.github.io/DDSQLtools/articles/Downloading-UNPD-data-into-R.html
## 
## get_seriesdata(): Get information about available details for a given series of data
## 
## get_dataprocess(): Get information about available data processes (DataProcessID)
## get_dataprocesstype(): idem but grouped by broaded data process types (DataProcessTypeID)
## 
## get_iitypes():     Get information about available indicators (IndicatorID) and indicatortypeids (IndicatorTypeId)
## get_indicators():     Get information about available indicators (IndicatorID)
## get_indicatortypes(): Get information about available indicators (IndicatorTypeID)
## 
## get_locations():     Get information about available locations (LocID)
## get_locationtypes(): Get information about available location types (LocAreaTypeID)
## 
## get_subgroups():     Get information about available sub-group-types (SubGroupTyp

## demogsurv (Install this package)
## https://github.com/mrc-ide/demogsurv
install.packages("devtools")
devtools::install_github("mrc-ide/demogsurv")


# (optional) Tools for aggregate demographic analysis ---------------------
## https://timriffe.github.io/DemoTools/

devtools::install_github("timriffe/DemoTools", force=TRUE)
devtools::install_github("timriffe/DDSQLTools", force=TRUE)

# Set library path --------------------------------------------------------
.libPaths() 
## Change library if need be:
# .libPaths("C:/Users/Stephen.Kisambira/AppData/Local/Programs/R/R-4.2.1/library") #for Windows

###+++++++++++++++
here::set_here(); here::dr_here(); here::here()
####____________________________________________________________________________


# Check if packages are installed (and install if not): -------------------
# https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
# https://stackoverflow.com/questions/26805267/r-subset-with-condition-using-in-or-which-one-should-be-used

# Run file ("abr-selection-functions.R") that contains specifications of directories and functions:

source("abr-selection-functions.R")

# ## If a package is installed, it will be loaded. If any are not, the missing package(s) will be installed from CRAN and then loaded.
# 
# ## Now load or install&load all packages
# package.check <- lapply(
#   my_packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )

dput(search()) # lists all parents of the global environment
search() # lists all parents of the global environment

# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Alternative loading or installing multiple packages:
#https://cran.r-project.org/web/packages/easypackages/easypackages.pdf
##packages("dplyr", "ggplot2", "RMySQL", "data.table", prompt = FALSE)
# easypackages::libraries(my_packages)
#packages(my_packages, prompt = FALSE)



# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # List of packages for session
# .packages = c("devtools", "data.table", "tictoc", "dplyr", "openssl", "openxlsx", "stringr")
# # Install CRAN packages (if not already installed)
# .inst <- .packages %in% installed.packages()
# if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)
# # Load packages into session 
# lapply(.packages, require, character.only=TRUE)
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Set server URL ----------------------------------------------------------
library(DDSQLtools)
serverURL <- "https://popdiv.dfs.un.org/DemoData/api/"
options(unpd_server = serverURL)
#names(options()); getOption("unpd_server")

# Get list of locations from Server ---------------------------------------
Locations <- data.table(get_locations(addDefault = "false",
                  includeDependencies = "false",
                  includeFormerCountries = "false"))
names(Locations)
Locations <- Locations[, .(LocID=PK_LocID, LocTypeID, LocName=Name)]


# Merge SDG regions with Locations ----------------------------------------
#SDG Regions

file.copy("C:/Users/Stephen.Kisambira/SDG_Data&reports/SDG2022Data/Input Data/RegionalGroupings2018.xlsx", "C:/Users/Stephen.Kisambira/SDG_Data&reports/SDG2022Data-TechnicalPaper/Input Data/RegionalGroupings2018.xlsx", overwrite = TRUE)

library(readxl)
excel_sheets("Input Data/RegionalGroupings2018.xlsx")
SDGregions.data <- read_excel("Input Data/RegionalGroupings2018.xlsx", sheet = "M49Locations")
names(SDGregions.data)

SDGregions.data <- SDGregions.data %>%
  select(., LocID, SDGregion )

names(Locations)
names(SDGregions.data)
library(dplyr)
merged1 <- data.frame(left_join(Locations, SDGregions.data, by = c("LocID")))

names(merged1)
df <- merged1 %>% subset(., is.na(SDGregion)); df; rm(df)

merged1$SDGregion[is.na(merged1$SDGregion) & merged1$LocName=="China, Taiwan Province of China"] <- "Eastern Asia and South-eastern Asia"

merged1$SDGregion[is.na(merged1$SDGregion) & (merged1$LocName=="Gaza Strip" | merged1$LocName=="West Bank")] <- "Western Asia and Northern Africa"

Locations <- merged1 %>%
  subset(., !is.na(SDGregion)) %>% 
  select(., LocID, LocTypeID, LocName, SDGregion); rm(merged1)

#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
saveRDS(Locations, file="Input Data/Locations.Rda")
##Then read it with:
# Locations <- readRDS(file="Input Data/Locations.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++


# DataCatalog -------------------------------------------------------------
DataCatalog <- data.table(get_datacatalog(addDefault = "false"))
names(DataCatalog)
DataProcess <- data.table(get_dataprocess(addDefault = "false")); names(DataProcess)
DataProcessType <- data.table(get_dataprocesstype(addDefault = "false")); names(DataProcessType)

## DataCatalog <- subset(DataCatalog, select=c("DataCatalogID", "LocID", "LocTypeID", "LocName", "DataProcessTypeID", "DataProcessType", "DataProcessTypeShortName", "DataProcessID", "DataProcess", "DataProcessShortName", "Name", "ShortName", "OfficialName", "OfficialShortName", "ReferencePeriod", "ReferenceYearStart", "ReferenceYearEnd", "ReferenceYearMid", "FieldWorkStart", "FieldWorkEnd", "FieldWorkMiddle", "ParentDataCatalogID", "isSubnational"))
DataCatalog[, FieldWorkStart := as.Date(FieldWorkStart, format="%m/%d/%Y")]
DataCatalog[, FieldWorkEnd   := as.Date(FieldWorkEnd, format="%m/%d/%Y")]
DataCatalog <- DataCatalog[is.na(LocTypeID)==FALSE]
setorder(DataCatalog, LocName, ShortName, ReferenceYearStart)

#'* Subset: ReferenceYearMid >= 2000 to cover the SDG reporting period *
names(DataCatalog.data)
str(DataCatalog.data)
DataCatalog <- DataCatalog %>%
  subset((ReferenceYearMid >= 2000 & isSubnational=="FALSE"))

#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
saveRDS(DataCatalog, file="Input Data/DataCatalog.data.Rda")
##Then read it with:
# DataCatalog.data <- readRDS(file="Input Data/DataCatalog.data.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++

## get_indicators():     Get information about available indicators (IndicatorID)
## get_iitypes():     Get information about available indicators (IndicatorID) and indicatortypeids (IndicatorTypeId)
indicators <- data.table(get_iitypes())

indicators <- indicators[, .(IndicatorID=PK_IndicatorID, IndicatorName=Name, IndicatorShortName=ShortName, UnitShortLabel, VariableType, FormatString, ComponentID, ComponentName=IndicatorType.ComponentName, IndicatorTypeID, IndicatorTypeName=IndicatorType.Name, IsComplete, SortOrder)]
setorder(indicators, SortOrder)

unique(indicators$ComponentName)
# indicators[ComponentName=="Population"]
indicators[ComponentName=="Fertility"]
# indicators[ComponentName=="Mortality"]
# indicators[ComponentName=="Life tables"]

## get_indicatortypes(): Get information about available indicators (IndicatorTypeID)

## get list of DataProcess
DataProcessType <- data.table(get_dataprocesstype())
DataProcess <- data.table(get_dataprocess())

DataProcess <- merge(DataProcess[, .(DataProcessID=PK_DataProcessID, DataProcessTypeID, Name, ShortName, SortOrder1)], DataProcessType[, .(PK_DataProcessTypeID, DataProcessTypeName=Name, DataProcessTypShortNamee=ShortName)], by.x="DataProcessTypeID", by.y="PK_DataProcessTypeID")

## example of selection for DataProcessTypeID
## 2=Census ; 11=Survey ; 12=Panel ; 8=PES
## 9=Register ; 10=Sample Registration System (SRS) ; 14=Administrative records
## 6=Estimate
DataProcess[DataProcessTypeID %in% c(2, 14, 11, 12, 8, 9, 6, 10), ]


## example of test using Uganda, either using LocID or name
myLocations <- Locations$LocID
# myLocations <- c(800)
# myLocations <- c(50)
## myLocations <- c("Uganda", "Senegal", "Ghana", "Bangladesh", "India")
## myLocations <- c("Belgium", "Mexico", "South Africa", "India")			  


DemoData_Query_ASFR <- function(myLocations, 
                       startYear, 
					   endYear) {
					   
options(scipen = 9999)

## step 1: download data from DemoData
## Query by chunk of locations
n_chunks <- 20
chunk_groups <- rep(1:n_chunks, length.out = length(myLocations))
cnty_groups <- split(myLocations, chunk_groups)
	   
# Loop through each location with `lapply`
myDT <- lapply(cnty_groups, function(x) {

  res <- get_recorddata(dataProcessTypeIds = c(2, 14, 11, 12, 8, 9, 6, 10),
                        startYear = startYear,
                        endYear = endYear,
                        indicatorIds = c(181),  ## ASFR5
                        locIds = x,             
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
						includeUncertainty = FALSE)
  return(res)
})

# Merge all separate country data frames into one data frame.
DT <- data.table(do.call(rbind, myDT))
DT$agesort <- NULL
## save(DT, file = paste0(mainDir, "\\ASFR_DB_raw-all.rda"))



## Step 2: "deduplicate" and filter/select between multiple series when multiple data sources or versions
## load(paste0(mainDir, "\\ASFR_DB_raw-all.rda"))

## filter out 
## DataTypeName==Model-based estimates
## DataTypeName==Model-based projections/extrapolations
## ModelPatternName=="Direct (Single year)"
DT <- DT[! (DataTypeID  %in% c(70, 71) | ModelPatternID == 60)]


DT[, RecallLag := FieldWorkMiddle - TimeMid]
DT[is.na(FieldWorkMiddle) | FieldWorkMiddle==0, RecallLag := 0]
DT[RecallLag < 0 & (DataTypeID %in% c(136, 148, 149, 150, 151, 169, 62, 153) |  DataProcessID %in% c(7)), RecallLag := 0.5]
DT[RecallLag < 0 & DataTypeID==135 & DataProcessTypeID==2, RecallLag := 0.5]
DT[RecallLag < 0 & DataTypeID %in% c(126, 152), RecallLag := 0.5]

## merge with DataTypeGroup2 groupings
DataTypeGroup2 <- data.table(get_datatypes())
DataTypeGroup2 <- DataTypeGroup2[,.(DataTypeID=PK_DataTypeID, DataTypeGroupID2, DataTypeGroupName2)]
DT <- merge(DT, DataTypeGroup2, by="DataTypeID")

## recode Birth histories
DT[DataProcess %in% c("DHS", "MICS", "RHS", "GFHS", "WFS", "PAPCHILD", "MICS/PAPFAM", "PAPFAM", "DHS-NS", "MICS-NS", "DHS/MICS", "DHS/PAPFAM", "MIS", "AIS") & DataTypeGroupName2=="Direct", DataTypeGroupName2 := "Birth histories"]
DT[DataProcess %in% c("DHS", "MICS", "RHS", "GFHS", "WFS", "PAPCHILD", "MICS/PAPFAM", "PAPFAM", "DHS-NS", "MICS-NS", "DHS/MICS", "DHS/PAPFAM", "MIS", "AIS") & DataTypeGroupName=="Model-based", DataTypeGroupName2 := "Birth histories"]
DT[DataProcess %in% c("DHS", "MICS", "RHS", "GFHS", "WFS", "PAPCHILD", "MICS/PAPFAM", "PAPFAM", "DHS-NS", "MICS-NS", "DHS/MICS", "DHS/PAPFAM", "MIS", "AIS") & DataTypeGroupName=="Model-based", DataTypeGroupName  := "Direct"]
DT[DataTypeGroupName2=="Birth histories",  DataTypeName := "Birth histories"]
DT[DataTypeGroupName2=="Birth histories",  DataTypeGroupID2 := 33]


## filter out PeriodSpan==1 if DataTypeName=="Cohort-completed fertility backdated by the mean age of childbearing"
## check also for Reverse Survival and Own-Children, and use periods rather than single year if both available
## create a DS5 hash key based on a composite set fields
DT[, MD5 := md5(paste(LocID, LocAreaTypeID, SubGroupCombinationID, DataCatalogID, DataSourceID, DataStatusID, StatisticalConceptID, ModelPatternID, DataTypeID, sep="-"))]
## 171 Cohort-completed fertility backdated by the mean age of childbearing  
## 152                                              Reverse survival method
## 124                                                  Own-children method
## 162                                   Computed from education statistics
## note education statistics corresponds to Reverse survival
DT <- merge(DT, unique(DT[DataTypeID %in% c(171, 152, 124, 162) & PeriodSpan > 1, .(MD5, DataTypeID, Period=1)]), by=c("MD5", "DataTypeID"), all.x = TRUE, all.y = FALSE)
## keep single year only if longer periods not available
DT <- DT[(! DataTypeID %in% c(171, 152, 124, 162)) | (DataTypeID %in% c(171, 152, 124, 162) & (PeriodSpan > 1 | (PeriodSpan <= 1 & is.na(Period))))]
DT$MD5 <- NULL
DT$Period <- NULL


## IDs to group series to check if UNPD FBH processing is available
## if both DHS/StatCOmpiler/report AND UNPD FBH processing is available for the same series, then keep only UNPD FBH
## create a DS5 hash key based on a composite set fields
## DT[, MD5 := md5(paste(LocID, LocAreaTypeID, SubGroupCombinationID, DataCatalogID, DataTypeGroupID2, AgeStart, AgeSpan, sep="-"))]
## DT <- merge(DT, DT[DataTypeGroupID2==33 & str_detect(DataSourceShortName, "FBH analysis")==TRUE, .(MD5, FBH=1)], by=c("MD5", "AgeStart", "AgeSpan"), all.x = TRUE, all.y = FALSE)
## 
## DT <- DT[IndicatorID==168 | (IndicatorID==167 & is.na(TFR1))]
## DT$MD5 <- NULL
## DT$TFR1 <- NULL


## deal with multiple version of series
deduplicates <- function(myDT) {	
	
	## sort records per location and year to order multiple observation by multi-criteria using sort orders
	setorder(myDT, LocID, TimeMid, DataCatalogShortName,
	StatisticalConceptSort, 
	DataStatusSort,
	DataProcessSort, DataProcessTypeSort, 
	DataSourceSort, -DataSourceYear, DataSourceShortName, 
	-DataTypeSort,
	DataReliabilitySort,
	ModelPatternName, PeriodGroupName, PeriodStart, PeriodSpan,
	AgeStart, AgeSpan)
		
	## subset key attributes to rank most authoritative series
	myDT[, MD5 := md5(paste(LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort, AgeStart, AgeSpan, sep="-"))]
	mySeries <- unique(myDT[, .(MD5, LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort, AgeStart, AgeSpan)])
	
	setorder(mySeries, LocID, DataCatalogShortName,
	StatisticalConceptSort, 
	DataStatusSort,
	DataProcessSort, DataProcessTypeSort, 
	DataSourceSort, -DataSourceYear, DataSourceShortName,
	-DataTypeSort,
	DataReliabilitySort,
	AgeStart, AgeSpan)
	
	## assign rank to each set of "dups"
	mySeries[, nrank := 1:.N, by=list(LocID, DataCatalogShortName, DataProcessSort, DataTypeSort, trunc(TimeMid), AgeStart, AgeSpan)]	
	mySeries <- mySeries[nrank==1]
		
	## keep only most authoritative versions (top #1) based on DataCatalogShortName, DataProcessSort, DataTypeSort
	myDT <- merge(myDT, mySeries[, .(MD5, AgeStart, AgeSpan)], by=c("MD5", "AgeStart", "AgeSpan")) 
	myDT$MD5 <- NULL
	
	return(myDT)
}

DT <- deduplicates(DT)

setorder(DT, LocID, LocAreaTypeID, SubGroupID1, DataCatalogShortName, DataSourceShortName,  DataSourceYear, DataStatusSort, StatisticalConceptSort, DataTypeSort, ModelPatternName, DataReliabilitySort, TimeMid, AgeStart, AgeSpan)
## save(DT, file = paste0(mainDir, "\\TFR_DB_all.rda"))

}

## example to call function
DT <- DemoData_Query_ASFR(myLocations, startYear=2000, endYear=2021)

#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
saveRDS(DT, file="DT.Rda")
##Then read it with:
# DT <- readRDS(file="DT.Rda")
#++++++++++++++++++++++++++++++++++++++++++++++

## keep only 5-year age groups (filter out total ASFR) and adolescent age groups
# DT <- DT[AgeSpan==5]
names(DT)
#str(DT)

DT$DataProcessType
# DT <- DT[AgeSpan==5 & (AgeStart==10 | AgeStart==15) & TimeDuration==3]
DT <- DT[AgeSpan==5 & (AgeStart==10 | AgeStart==15)]

## rbwoxse results
DT[, .( LocID, LocName,  TimeMid, AgeStart, AgeSpan, DataCatalogID, DataCatalogShortName, DataSourceShortName, PeriodSpan, PeriodGroupName, TimeLabel, TimeDuration, RecallLag, DataValue, DataProcessType, DataReliabilityName, DataTypeName,  DataSourceYear, ModelPatternName )] -> mm

mm <- data.frame(mm)

# DT[(AgeStart==10 | AgeStart==15), .( LocID, LocName, AgeStart, AgeSpan, DataCatalogID, DataCatalogShortName, DataSourceShortName, PeriodSpan, PeriodGroupName, TimeLabel, TimeDuration, TimeMid, RecallLag, DataValue, DataProcessType, DataReliabilityName, DataTypeName,  DataSourceYear, ModelPatternName )]

#++++++++++++++++++++++++++++++++++++
#Rename reference year:
mm <- mm %>%
plyr::rename(., c("TimeMid"="RefYear"))
mm$RefYear <- trunc(mm$RefYear)
dput(names(mm))
setorder(mm, LocID, RefYear, RecallLag)
#++++++++++++++++++++++++++++++++++++

##___________________________
# Rank by data quality
str(mm)
library(dplyr)
mm <- mm %>% mutate(
  DataQuality = case_when(
  # Registration UNSD & WPP2019:
  DataReliabilityName == "High quality" ~ 1,
  DataReliabilityName == "Fair" ~ 2,
  DataReliabilityName == "Low" ~ 3,
  DataReliabilityName == "Unknown" ~ 4
  ))


##___________________________
#Remove duplicated reference years:
dput(names(mm))
mm <- mm %>% #filter(., LocID==800) %>%
  group_by(LocID, AgeStart, RefYear) %>%
  dplyr::arrange(LocID, AgeStart, RefYear, RecallLag, DataQuality) %>%  
  #Sort on RecallLag because distinct will keep the first row of a duplicate set and discard the rest
  dplyr::distinct(AgeStart, RefYear, .keep_all = T) %>%
  ungroup() %>% 
#  filter(DataReliabilityName == "High quality") %>%
  plyr::rename(., c("DataValue"="ABR")) %>%
  select("LocID", "LocName", "RefYear", "AgeStart", "ABR", "RecallLag", everything())

##__________________________
library(dplyr)
names(mm)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Select data sources with the largest number of series:
#https://datacornering.com/dplyr-error-n-must-only-be-used-inside-dplyr-verbs/
require(plyr)
require(dplyr)

#Age group 10-14:
dd_10 <- mm %>% 
  filter(AgeStart==10) %>%  
  ungroup() %>%
  group_by(LocID, AgeStart, DataProcessType) %>% 
  arrange(LocID, AgeStart, DataProcessType) %>% 
  dplyr::mutate(count1=row_number()) %>%   
  dplyr::mutate(count1_max = max(count1)) %>% 
    ungroup() %>% 
#https://stackoverflow.com/questions/30770006/dplyr-max-value-in-a-group-excluding-the-value-in-each-row
  group_by(LocID, AgeStart) %>% 
  dplyr::mutate(max = ifelse(count1_max == max(count1_max), count1_max, NA)) %>% 
  filter(!is.na(max)) %>%
  arrange(LocID, AgeStart, RefYear) %>%   
  ungroup() %>%
    select(-c(count1, count1_max, max))

#Age group 15-19:
dd_15 <- mm %>% 
#  filter(AgeStart==15 & LocName=="Bangladesh") %>%  
  filter(AgeStart==15) %>%  
  ungroup() %>%
  dplyr::group_by(LocID, AgeStart, DataProcessType) %>% 
  arrange(LocID, AgeStart, DataProcessType) %>% 
  dplyr::mutate(count1=row_number()) %>%   
  dplyr::mutate(count1_max = max(count1)) %>% 
    ungroup() %>% 
#https://stackoverflow.com/questions/30770006/dplyr-max-value-in-a-group-excluding-the-value-in-each-row
#  group_by(LocID, AgeStart) %>% 
  dplyr::group_by(LocID) %>%   
  dplyr::mutate(max = ifelse(count1_max == max(count1_max), count1_max, NA)) %>% 
  filter(!is.na(max)) %>%
  arrange(LocID, AgeStart, RefYear) %>%   
  dplyr::ungroup() %>%
    select(-c(count1, count1_max, max))


# Save MaxDataSourceSeries data for each age group:
saveRDS(dd_10, file="Input Data/MaxDataSourceSeries_10.Rda")
saveRDS(dd_15, file="Input Data/MaxDataSourceSeries_15.Rda")

# Merge all separate country data frames into one data frame.
MaxDataSourceSeries.data <- rbind(dd_10, dd_15)
rm(dd_10, dd_15)

#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
saveRDS(MaxDataSourceSeries.data, file="Input Data/MaxDataSourceSeries.data.Rda") #Nov 10, 2021: 

##Then read it with:
# MaxDataSourceSeries.data <- readRDS(file="Input Data/MaxDataSourceSeries.data.Rda")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Select all data  for ABR 10-14 and ABR 15-19 irrespective which data sources are preponderant, giving priority to the availability of data on ABR 10-14:

# dput(names(mm))

names(mm)
names(MaxDataSourceSeries.data)

AllDataSourcesDemoData.data <- mm
 rm(mm,DT)
#++++++++++++++++++++++++++++++++++++++++++++++
##Save the returned data using saveRDS:
saveRDS(AllDataSourcesDemoData.data, file="Input Data/AllDataSourcesDemoData.data.Rda")

##Then read it with:
# AllDataSourcesDemoData.data <- readRDS(file="Input Data/AllDataSourcesDemoData.data.Rda")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

