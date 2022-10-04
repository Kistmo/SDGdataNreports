## R wrapper to connect to DemoData and download/filter ASFR data

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

## (optional) Tools for aggregate demographic analysis
## https://timriffe.github.io/DemoTools/

# devtools::install_github("timriffe/DemoTools", force=TRUE)
# devtools::install_github("timriffe/DDSQLTools", force=TRUE)

# List of packages for session
.packages = c("devtools", "data.table", "tictoc", "dplyr", "openssl", "openxlsx", "stringr")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)
# Load packages into session 
lapply(.packages, require, character.only=TRUE)

library(DDSQLtools)
serverURL <- "https://popdiv.dfs.un.org/DemoData/api/"
options(unpd_server = serverURL)


## get list of locations from Server
Locations <- data.table(get_locations(addDefault = "false",
                  includeDependencies = "false",
                  includeFormerCountries = "false"))
Locations <- Locations[, .(LocID=PK_LocID, LocTypeID, LocName=Name)]

DataCatalog <- data.table(get_datacatalog(addDefault = "false"))
DataProcess <- data.table(get_dataprocess(addDefault = "false"))
DataProcessType <- data.table(get_dataprocesstype(addDefault = "false"))

## DataCatalog <- subset(DataCatalog, select=c("DataCatalogID", "LocID", "LocTypeID", "LocName", "DataProcessTypeID", "DataProcessType", "DataProcessTypeShortName", "DataProcessID", "DataProcess", "DataProcessShortName", "Name", "ShortName", "OfficialName", "OfficialShortName", "ReferencePeriod", "ReferenceYearStart", "ReferenceYearEnd", "ReferenceYearMid", "FieldWorkStart", "FieldWorkEnd", "FieldWorkMiddle", "ParentDataCatalogID", "isSubnational"))
DataCatalog[, FieldWorkStart := as.Date(FieldWorkStart, format="%m/%d/%Y")]
DataCatalog[, FieldWorkEnd   := as.Date(FieldWorkEnd, format="%m/%d/%Y")]
DataCatalog <- DataCatalog[is.na(LocTypeID)==FALSE]
setorder(DataCatalog, LocName, ShortName, ReferenceYearStart)

DataCatalog[LocName=="Uganda"]


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
myLocations <- c(800)
## myLocations <- c("Uganda", "Senegal", "Ghana", "Bangladesh", "India")
			  


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
DT <- DemoData_Query_ASFR(myLocations, startYear=2000, endYear=2020)

## keep only 5-year age groups (filter out total ASFR)
DT <- DT[AgeSpan==5]

## rbwoxse results
DT[, .(LocID, LocName, AgeStart, AgeSpan, DataCatalogID, DataCatalogShortName, DataSourceShortName, DataSourceYear, DataTypeName, ModelPatternName, PeriodGroupName, TimeLabel, TimeMid,  DataValue)]
DT[AgeStart==10, .(LocID, LocName, AgeStart, AgeSpan, DataCatalogID, DataCatalogShortName, DataSourceShortName, DataSourceYear, DataTypeName, ModelPatternName, PeriodGroupName, TimeLabel, TimeMid,  DataValue)]

DT[AgeStart==10 & DataCatalogShortName=="2016 DHS", .(LocID, LocName, AgeStart, AgeSpan, DataCatalogID, DataCatalogShortName, DataSourceShortName, DataSourceName, DataSourceYear, DataTypeName, ModelPatternName, PeriodGroupName, TimeLabel, TimeMid,  DataValue)]

## short list of data catalog with data
unique(DT[AgeStart==15, DataCatalogShortName])
DT[AgeStart==15, .(LocID, LocName, AgeStart, AgeSpan, DataCatalogID, DataCatalogShortName, DataSourceShortName, DataSourceName, DataSourceYear, DataTypeName, ModelPatternName, PeriodGroupName, TimeLabel, TimeMid,  DataValue)]


## export results to Excel for further inspection / data browsing
hs1 <- createStyle(fgFill = "#4F81BD", halign = "LEFT", textDecoration = "Bold",
                   border = "Bottom", fontColour = "white")
		
l <- list("ASFR5" = DT)
write.xlsx(l, file="ASFR5.xlsx"	, asTable = TRUE, colWidths="auto", headerStyle = hs1, tableStyle = "TableStyleLight2")
