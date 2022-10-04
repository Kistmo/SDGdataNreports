
####________________________________________________________________________________________
#---
# title: "ABR (10-14 and 15-19) datasets"
# purpose: to select ABR 1519 for SDG reporting; calculate the annual change in ABR 15-19
# author: "Stephen Kisambira"
# date: "12/01/2022"
# output: html_document, chart; excel file
#---
# House-keeping functions -------------------------------------------------
rm(list=ls()) #Remove all objects.
Sys.info()

source("abr-selection-functions.R")
####___________________________________________________________________________
gc(); cat("\014"); clearhistory(); ##rm_history_database

# Data sets ---------------------------------------------------------------
here::dr_here()
# Read in the most recent selected data to compute annual change.
library(data.table); library(readxl)

excel_sheets("Output Data/ABR_SDG2022_SelectedData.xlsx")

ABR_SDG2022_SelectedDataForChecking.data <- read_excel("Output Data/ABR_SDG2022_SelectedData.xlsx",sheet = "SelectedDataForChecking2")

names(ABR_SDG2022_SelectedDataForChecking.data)

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Extract Unique Elements:
abr1519country.list <- ABR_SDG2022_SelectedDataForChecking.data %>% 
  subset(AgeStart==15 ) %>%
  # dplyr::group_by(LocID) %>%
  # ##Remove countries with one data point;
  # dplyr::count(LocID, LocName) %>% subset(., n > 1) %>%
  # ungroup() %>%
  dplyr::distinct(LocID, .keep_all = TRUE) %>%
  select(., LocID, LocName) %>%
  dplyr::arrange(LocName)

library(dplyr)
library(data.table)

###+++++++++++++++++++
#Split data by age group:
#Subset ABR1519 data:
df_1519 <- ABR_SDG2022_SelectedDataForChecking.data %>%
  subset(AgeStart==15)

names(df_1519)

###+++++++++++++++++++
#Subset ABR1014 data:
df_1014 <- ABR_SDG2022_SelectedDataForChecking.data %>%
  subset(AgeStart==10)

names(df_1014)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


mm <- as.data.frame(df_1519) %>%
  dplyr::arrange(LocName, RefYear) %>%
  # Make sure that mm actually is a data.table (check with class(mm)).
  #setkeyv(., cols = c("Country", "RefYear")) %>% 
  group_by(LocName) %>%
  dplyr::mutate(row_num = 1:n()) %>%
  ungroup()

# Show countries with only one data point: ------------------------------
names(mm)
library(dplyr)
onedatapointcountries.data <- mm %>%
  group_by(LocName) %>%
  dplyr::mutate(row_numbers = n()) %>% #Create variable with total count of observations in a country
  #https://stackoverflow.com/questions/31661704/remove-group-from-data-frame-if-at-least-one-group-member-meets-condition
  filter(row_numbers < 2)  %>% 
  ungroup() %>%
  distinct(., LocName, .keep_all = TRUE)

onedatapointcountries.data$LocName

##_________________________________________________________________________
##_________________________________________________________________________
names(mm)
unique(mm$SDG2021Status)
#help(":=")

# Calculate annual percentage change: -------------------------------------
#mm <- docountry
mm <- data.table(mm) %>%
  setkeyv(., cols = c("LocName" , "RefYear")) 

#Annual change in ABR:
mm[, abr.lag1annualchange := (ABR - lag(ABR, 1 )) / (RefYear - lag(RefYear, 1 )), by = c("LocName" )]
mm[, pct.abr.lag1annualchange := abr.lag1annualchange / lag(ABR, 1 )*100, by = c("LocName" )]

require(Hmisc)
##_______________________________________
#Reverse outlier value:
mm <- mutate(mm, pct.abr.lag1annualchange = ifelse(is.na(pct.abr.lag1annualchange) & 
abs(lead(pct.abr.lag1annualchange,1)) >= 25, 1000, pct.abr.lag1annualchange))

mm <- mutate(mm, pct.abr.lag1annualchange = ifelse(lag(pct.abr.lag1annualchange) >= 1000 & 
          !is.na(lag(pct.abr.lag1annualchange)), 0.0001, pct.abr.lag1annualchange))
##_______________________________________

mm <- mutate(mm, pct.lag1annualchangeoutlier = ifelse(abs(pct.abr.lag1annualchange) >=25 & 
                  !abs(pct.abr.lag1annualchange) == Inf, "Outlier", NA))

round2(mm$abr.lag1annualchange, 1) -> mm$abr.lag1annualchange
round2(mm$pct.abr.lag1annualchange, 2) -> mm$pct.abr.lag1annualchange
round2(mm$ABR, 1) -> mm$ABR

range(mm$RefYear)
#str(mm)
# vignette("magrittr")
# ####______________________________________________________________________________
names(mm)
# mm <- mutate(mm, pct_change = pct.abr.lag1annualchange) %>% #For plotting.
mm <- mutate(mm, pct_change = pct.abr.lag1annualchange)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@	OUTPUT TO EXCEL	@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
####_______________________________________________________________________________
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Output that final selected data to an Excel file.
## Extract Unique Elements:
#abr1519country.list <- ABR_SDG2022_SelectedDataForChecking.data %>%
mm <- as.data.frame(mm)
abr1519country.list <- mm %>%  
  subset(., select = c(LocID, LocName)) %>%
  #Remove countries with one data point;
  dplyr::count(LocID, LocName) %>% subset(., n > 1) %>% #Comment out to include all countries
  #dplyr::distinct(LocName, .keep_all = TRUE) %>%
  unique(., by=c("LocID")) %>%  
  subset(., select = c(LocID, LocName)) %>%
  arrange(LocName)

#dput((abr1519country.list$LocName))
#Remove object "mm_sans_outliers" if it exists, so we can create a new one:
if( exists("mm_sans_outliers") ) remove("mm_sans_outliers")

n=nrow(abr1519country.list)

gc(); cat("\014"); clearhistory(); 
names(mm)
#n=1
# i=47
for(i in 1:n){
  docountry <- subset(mm, LocName==abr1519country.list$LocName[[i]])
  
#Deselect outliers:
docountry_sans_outliers <- docountry %>%
  # subset(.,pct.lag1annualchangeoutlier!="Outlier" | is.na(pct.lag1annualchangeoutlier) | pct.lag1annualchangeoutlier=="NA" | SDG2021Status=="Yes")
    subset(.,pct.lag1annualchangeoutlier!="Outlier"| is.na(pct.lag1annualchangeoutlier))

str(docountry)

#Select outliers:
names(docountry)
docountry_avec_outliers <- docountry %>%
  subset(., pct.lag1annualchangeoutlier == "Outlier")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  if (!exists("mm_sans_outliers")){
    mm_sans_outliers <- docountry_sans_outliers
  } else {
    #Rebind datasets to whole
    mm_sans_outliers <- rbind(mm_sans_outliers,docountry_sans_outliers)
    #rm(docountry_sans_outliers, docountry)
  }
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++  

if (!exists("mm_avec_outliers")){
  mm_avec_outliers <- docountry_avec_outliers
} else {
  #Rebind datasets to whole
  mm_avec_outliers <- rbind(mm_avec_outliers, docountry_avec_outliers)
  #rm(docountry_avec_outliers, docountry)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
  }

#rm(mm_sans_outliers)
names(df_1014)
names(mm_sans_outliers)

FinalSelectedData <- rbind.fill(df_1014, mm_sans_outliers) %>%
  select(., -c("row_num", "abr.lag1annualchange", "pct.abr.lag1annualchange","pct.lag1annualchangeoutlier", "pct_change"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
cat("\014"); gc(verbose = getOption("verbose"), reset = TRUE); clearhistory()
###_________________________________________________________________________________

#### Write output to Excel file --------------------------------
#https://stackoverflow.com/questions/54977747/r-error-in-as-vectorx-character-when-using-openxlsx-to-write-excel
library(openxlsx)
filename <- paste(output.dir,"/ABR_SDG2022_SelectedData.xlsx", sep = "")
wb <- openxlsx::loadWorkbook(filename)
options("openxlsx.borderStyle" = "thin")

#++++++++++++++++++++++++++++++++++++++++++++++
#Read in file "ABR_SDG2022_SelectedData.xlsx:
library(readxl)

#Check existing worksheets:
excel_sheets("Output Data/ABR_SDG2022_SelectedData.xlsx")



#Remove and add worksheet:
#https://www.r-bloggers.com/2018/10/modifying-excel-files-using-openxlsx/
if (('ABR_SDG2022_ByChangeInABR1519' %in% names(wb))) removeWorksheet(wb, 'ABR_SDG2022_ByChangeInABR1519')

if (('ABR_SDG2022_FinalSelection' %in% names(wb))) removeWorksheet(wb, 'ABR_SDG2022_FinalSelection')

if (('SDG2022_OutlierABR1519' %in% names(wb))) removeWorksheet(wb, 'SDG2022_OutlierABR1519')

openxlsx::addWorksheet(wb, "ABR_SDG2022_ByChangeInABR1519", tabColour = "purple",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))

openxlsx::addWorksheet(wb, "SDG2022_OutlierABR1519", tabColour = "magenta",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))

openxlsx::addWorksheet(wb, "ABR_SDG2022_FinalSelection", tabColour = "green",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))


#https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
openxlsx::writeData(wb, "ABR_SDG2022_ByChangeInABR1519", 
        "Output generated from calculating the annual change in ABR 15-19 and removing data points with percentage change exceeding 25 per cent per year", startCol = 1, startRow = 1)

openxlsx::writeDataTable(wb, "ABR_SDG2022_ByChangeInABR1519", mm,
               startRow = 3, startCol = 1, tableStyle = "TableStyleLight9")
#setColWidths(wb, 'ABR_SDG2022_ByChangeInABR1519', cols = 1:ncol(mm), widths = 'auto')

openxlsx::writeDataTable(wb, "SDG2022_OutlierABR1519", mm_avec_outliers,
               startRow = 1, startCol = 1, tableStyle = "TableStyleLight5")

openxlsx::writeDataTable(wb, "ABR_SDG2022_FinalSelection", FinalSelectedData,
               startRow = 1, startCol = 1, tableStyle = "TableStyleLight11")
#setColWidths(wb, 'ABR_SDG2022_FinalSelection', cols = 1:ncol(FinalSelectedData), widths = 'auto')

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Save workbook:
openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

rm(filename)
####________________________________________________________________________________


###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###____________________________ END OF FILE	________________________________________
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###



