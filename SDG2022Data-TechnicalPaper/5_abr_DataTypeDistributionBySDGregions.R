
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

options(scipen=999, digits=9)

####______________________________________________________________________________
gc(); cat("\014"); clearhistory(); ##rm_history_database

# Libraries ---------------------------------------------------------------
####_________________________________________________________________________

.libPaths() 
##Uncomment following line and edit libPaths if need be:
#.libPaths("/Library/Frameworks/R.framework/Versions/4.2/Resources/library") #for Mac

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##++++++++  Data type distribution by SDG region and country +++++++++++++++++++++++++
####_________________________________________________________________________________

#++++++++++++++++++++++++++++++++++++++++++++++
#Read in file "ABR_SDG2022_SelectedData.xlsx:
library(readxl)

#AlDatapoints file used for SDG 2021 reporting/selection:
excel_sheets("Output Data/ABR_SDG2022_SelectedData.xlsx")

df <- read_excel("Output Data/ABR_SDG2022_SelectedData.xlsx",sheet = "ABR_SDG2022_FinalSelection")

names(df)

# Merge SDG regions with Locations ----------------------------------------
#SDG Regions
Locations <- readRDS(file="Input Data/Locations.Rda") %>%
  select(-c(LocName))

levels(Locations$SDGregion)
Locations$SDGregion <- as.factor(as.character(Locations$SDGregion))
levels(Locations$SDGregion)
names(Locations)

names(df)
library(dplyr)
df <- data.frame(left_join(df, Locations, by = c("LocID")))
df <- df %>%
  subset(., !is.na(SDGregion))

#++++++++++++++++++++++++++++++++++++++++++++++

#Rename levels:
names(df)
levels(df$SDGregion)
df$SDGregion <- as.factor(as.character(df$SDGregion))
levels(df$SDGregion)[levels(df$SDGregion)=="Oceania excluding Australia and New Zealand"] <- 
  "Oceania (excl. Australia and New Zealand)"
levels(df$SDGregion)


library(gsubfn)
library(mgsub)

#Rename "DataProcessType" to "DataSourceType":
#https://stackoverflow.com/questions/35444292/r-rename-multiple-columns-by-extracting-part-of-the-name
names(df) <- sub("DataProcessType", "DataSourceType", names(df))

names(df)

#Data based on all data sources:
#ABR 10-14:

df1014 <-  df %>%
  filter(AgeStart==10) %>%
  #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    # dplyr::rename_all(
    # ~stringr::str_replace_all(., "_1014", "")
    # ) %>%
  group_by(LocID, RefYear) %>%
    arrange(RefYear, RecallLag, ABR) %>%
    dplyr::distinct(RefYear, !is.na(ABR), .keep_all = T)  %>%
    filter(!is.na(ABR)) %>%
  ungroup()
#___________________________________
#+++++++++++__________++++++++++++++++

#___________________________________
#Data based on all data sources:
#ABR 15-19:

df1519 <-  df %>%
  filter(AgeStart==15) %>%  
    # #https://www.wtmatthias.com/2017/10/04/find-replace-in-var-names/
    # dplyr::rename_all(
    # ~stringr::str_replace_all(., "_1519", "")
    # ) %>%
  group_by(LocID, RefYear) %>%
    arrange(RefYear, RecallLag, ABR) %>%
    dplyr::distinct(RefYear, !is.na(ABR), .keep_all = T)  %>%
    filter(!is.na(ABR)) %>%
  ungroup()


# Count number of countries or areas: -------------------------------------
names(df1014)
## Extract Unique Elements with at least one data point:
library(dplyr)
df1014country.list <- df1014 %>%
  subset(., select = c(LocID, LocName)) %>%
  #Remove countries with one data point;
  # dplyr::count(LocName) %>% subset(., n > 1) %>%  
  dplyr::count(LocName) %>% subset(., n > 0) %>%
  #dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::arrange(LocName)

df1519country.list <- df1519 %>%
  subset(., select = c(LocID, LocName)) %>%
  #Remove countries with one data point;
  # dplyr::count(LocName) %>% subset(., n > 1) %>%  
  dplyr::count(LocName) %>% subset(., n > 0) %>%
  #dplyr::distinct(., LocID, .keep_all = TRUE) %>%
  dplyr::arrange(LocName)

unique(df1014$LocName) #They are 207 (204 with at least one data point)
unique(df1519$LocName) #They are 215 (213 with at least one data point)
range(df1014$RefYear) #2000 - 2021
range(df1519$RefYear) #2000 - 2021

# Distribution of data points\ ---------------------------------
####_______________________________________________________________________________________
# str(df)
# vignette('programming')
library(plyr)
library(dplyr)
names(df1014)

require(dplyr)
dff <- df1014
kk1 <- dff %>% dplyr::count(DataSourceType)
#unique(df$LocName)

# kk1b <- df %>% count(DataSourceType, Country) %>% 
#   subset(.,select = -c(Country, n)) %>% 
#     count(DataSourceType)

kk1b <- dff %>% count(DataSourceType, LocName) %>% 
  select(-c(LocName, n)) %>% 
  count(DataSourceType)
  
kk1 <- merge(kk1,kk1b, by = c("DataSourceType"), all.x = TRUE) ; rm(kk1b)

library(janitor)
kk1 <- kk1 %>%
  mutate("Data Points (per cent)" = n.x, "Countries (per cent)" = n.y) %>%
  adorn_totals("row") %>%
  adorn_percentages("col",,,"Data Points (per cent)")  %>%
  adorn_percentages("col",,,"Countries (per cent)") %>%
  adorn_pct_formatting(digits = 1,
                       affix_sign = FALSE,,,"Data Points (per cent)") %>%
  adorn_pct_formatting(digits = 1,
                       affix_sign = FALSE,,,"Countries (per cent)") %>%
  plyr::rename(., c("DataSourceType"="Data Source Type","n.x"="Number of Data Points",
                    "n.y"="Number of Countries"))

str(kk1)
kk1$'Data Points (per cent)' <- as.numeric(as.character(kk1$'Data Points (per cent)'))
kk1$'Countries (per cent)' <- as.numeric(as.character(kk1$'Countries (per cent)'))
kk1$`Age group` <- "10-14 years"
df1014count <- kk1; rm(kk1,dff)

nrow(df1014country.list)
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

names(df1519)

dff <- df1519
kk1 <- dff %>% count(DataSourceType)
#unique(df$LocName)

# kk1b <- df %>% count(DataSourceType, Country) %>% 
#   subset(.,select = -c(Country, n)) %>% 
#     count(DataSourceType)
kk1b <- dff %>% count(DataSourceType, LocName) %>% 
  select(-c(LocName, n)) %>% 
  count(DataSourceType)
  
kk1 <- merge(kk1,kk1b, by = c("DataSourceType"), all.x = TRUE) ; rm(kk1b)

library(janitor)
kk1 <- kk1 %>%
  mutate("Data Points (per cent)" = n.x, "Countries (per cent)" = n.y) %>%
  adorn_totals("row") %>%
  adorn_percentages("col",,,"Data Points (per cent)")  %>%
  adorn_percentages("col",,,"Countries (per cent)") %>%
  adorn_pct_formatting(digits = 1,
                       affix_sign = FALSE,,,"Data Points (per cent)") %>%
  adorn_pct_formatting(digits = 1,
                       affix_sign = FALSE,,,"Countries (per cent)") %>%
  plyr::rename(., c("DataSourceType"="Data Source Type","n.x"="Number of Data Points",
                    "n.y"="Number of Countries"))

str(kk1)
kk1$'Data Points (per cent)' <- as.numeric(as.character(kk1$'Data Points (per cent)'))
options(digits=9)
kk1$'Countries (per cent)' <- as.numeric(as.numeric(kk1$'Countries (per cent)'))
kk1$`Age group` <- "15-19 years"
df1519count <- kk1; rm(kk1,dff)
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Bind the data for ABR (10-14 and 15-19 years) and rearrange order of columns:
#https://stackoverflow.com/questions/28017141/select-the-last-n-columns-of-data-frame-in-r
library(dplyr)
dfcount <- rbind(df1014count,df1519count) %>%
       select(`Age group`, everything())

rm(df1014count,df1519count)

#Data distribution by SDG regions:
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
names(df1014)
kk4 <- df1014 %>% count(DataSourceType, SDGregion)  %>%
  # Group by ABRlevel and Region, then count number in each group
  mutate("Per cent" = n) %>%
  adorn_totals("row") %>%
  adorn_percentages("col",,,"Per cent") %>%
  adorn_pct_formatting(digits = 1,
                       affix_sign = FALSE,,,"Per cent")
names(kk4)
#Rename vectors/variables/headers

kk4 <- kk4 %>%
  plyr::rename(., c("DataSourceType"="Data Source Type",
                    "n"="Number of Data Points"))
str(kk4)
kk4$'Per cent' <- as.numeric(as.character(kk4$'Per cent'))
kk4$`Age group` <- "10-14 years"
df1014countSDGregions <- kk4; rm(kk4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
names(df1519)
kk4 <- df1519 %>% count(DataSourceType, SDGregion)  %>%
  # Group by ABRlevel and Region, then count number in each group
  mutate("Per cent" = n) %>%
  adorn_totals("row") %>%
  adorn_percentages("col",,,"Per cent") %>%
  adorn_pct_formatting(digits = 1,
                       affix_sign = FALSE,,,"Per cent")
names(kk4)
#Rename vectors/variables/headers

kk4 <- kk4 %>%
  plyr::rename(., c("DataSourceType"="Data Source Type",
                    "n"="Number of Data Points"))
str(kk4)
kk4$'Per cent' <- as.numeric(as.character(kk4$'Per cent'))
kk4$`Age group` <- "15-19 years"
df1519countSDGregions <- kk4; rm(kk4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Bind the data for ABR (10-14 and 15-19 years) and rearrange order of columns:
#https://stackoverflow.com/questions/28017141/select-the-last-n-columns-of-data-frame-in-r
library(dplyr)
dfcountSDGregions <- rbind(df1014countSDGregions, df1519countSDGregions) %>%
       select(`Age group`, everything())

rm(df1014countSDGregions, df1519countSDGregions)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Datapoint count by country and data source type -------------------------
names(df1014)
unique(df1014$DataSourceType)
kk5 <- df1014 %>% count(LocName, DataSourceType)

kk5 %>% count(n)
kk5$`Age group` <- "10-14 years"

kk5 <- kk5 %>%
  #group_by(Country) %>%
  pivot_wider(names_from = c(DataSourceType),
              values_from = c(n),
              values_fill = list(n = 0)) %>%
  #ungroup() %>%
  #data.table::setcolorder(., c(1, 5, 2, 4, 3, 6)) %>% # Re-order the columns
  select(LocName, `Age group`, Registration, Survey, Estimate, Census) %>% # Re-order the columns
  janitor::adorn_totals("col") %>%
  janitor::adorn_totals("row")

df1014countDataSourceType <- kk5; rm(kk5)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


names(df1519)
unique(df1519$DataSourceType)
kk5 <- df1519 %>% count(LocName, DataSourceType)

kk5 %>% count(n)
kk5$`Age group` <- "15-19 years"

kk5 <- kk5 %>%
  #group_by(Country) %>%
  pivot_wider(names_from = c(DataSourceType),
              values_from = c(n),
              values_fill = list(n = 0)) %>%
  #ungroup() %>%
  #data.table::setcolorder(., c(1, 5, 2, 4, 3, 6)) %>% # Re-order the columns
  select(LocName, `Age group`, Census, Estimate,Registration, Survey) %>% #Re-order the columns
  janitor::adorn_totals("col") %>%
  janitor::adorn_totals("row")

df1519countDataSourceType <- kk5; rm(kk5)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###__________________________________________________________________________
###### Figure 1. Distributions of data points by data source type and SDG regions:
# A grouped barplot

# bubu1014 <- df1014 %>%
#   subset(., is.na(DataSourceType) )

names(df1014)

Fig1 <-  df1014 %>% dplyr::count(DataSourceType, SDGregion) %>%
  mutate(SDGregion=as.factor(SDGregion)) %>%
  mutate(SDGregion = forcats::fct_reorder(SDGregion, n)) %>%
  ggplot( aes(x=reorder(SDGregion, n) , y=n,
              fill=factor(DataSourceType,
                          levels=c("Registration","Estimate","Survey","Census")))) +
  geom_bar(stat="identity", width = 0.85, position = position_stack(reverse = TRUE)) +
  scale_color_viridis(discrete=TRUE, direction = 1) +
  #theme_ipsum() +
  #labs(title = "Fig1_Distribution-of-\nABR_datapoints-by-SDG-regions") +
  #ylab("Number of Data Points") + 
  ylab("") +   
  #xlab("SDG Regions") +
  xlab("") +  
  coord_flip() +
   theme_bw(18) +
  theme(
    text=element_text(size=10),
    legend.position="top", legend.direction = "horizontal",
    panel.spacing = unit(0.1, "lines"), legend.margin=margin(),
    strip.text.x = element_text(size = 8), legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) #+ 
  #http://www.sthda.com/english/wiki/ggplot2-texts-add-text-annotations-to-a-graph-in-r-software
  #annotate(geom="text", x="Eastern Asia and South-eastern Asia", y=600, label="ABR (10-14 years)",
  #            color="red")

Fig1
#rem(Fig1,Fig2)
###__________________________________________________________________________
names(df1519)
# bubu1519 <- df1519 %>%
#   subset(., is.na(DataSourceType) )

Fig2 <-  df1519 %>% dplyr::count(DataSourceType, SDGregion) %>%
  mutate(SDGregion=as.factor(SDGregion)) %>%
  mutate(SDGregion = forcats::fct_reorder(SDGregion, n)) %>%
  ggplot( aes(x=reorder(SDGregion, n) , y=n,
              fill=factor(DataSourceType,
                          levels=c("Registration","Estimate","Survey","Census", "Panel")))) +
  geom_bar(stat="identity", width = 0.85, position = position_stack(reverse = TRUE)) +
  scale_color_viridis(discrete=TRUE, direction = 1) +
  #theme_ipsum() +
  #labs(title = "Fig1_Distribution-of-\nABR_datapoints-by-SDG-regions") +
  ylab("Number of Data Points") + 
  # xlab("SDG Regions") +
  xlab("") +  
  coord_flip() +
  theme(
    text=element_text(size=10),
    legend.position="top", legend.direction = "horizontal",
    panel.spacing = unit(0.1, "lines"), legend.margin=margin(),
    strip.text.x = element_text(size = 8), legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) #+ 
  # #http://www.sthda.com/english/wiki/ggplot2-texts-add-text-annotations-to-a-graph-in-r-software
  # annotate(geom="text", x="Eastern Asia and South-eastern Asia", y=600, label="ABR (15-19 years)",
  #             color="red")

Fig2

# if( exists('Fig1') & exists('Fig2') ) {
#   print(Fig1 + Fig2)
# } else {
#     #skip
#   }


# Combine Multiple GGPlots in One Graph -----------------------------------
#http://www.sthda.com/english/articles/32-r-graphics-essentials/126-combine-multiple-ggplots-in-one-graph/

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

figure <- ggarrange(Fig1, Fig2,
                    labels = c("ABR (10-14 years)", "ABR (15-19 years)"),
                    font.label = list(size = 11, face = "plain", color ="magenta"),
                    ncol = 1, nrow = 2,
  common.legend = TRUE, legend = "bottom",
  hjust = -0.2, vjust = -0.5)
figure

annotate_figure(
  figure,
  top = text_grob("",
                  color = "red", face = "plain", size = 14),
  bottom = text_grob("Data source: \n UN Population Division, DemoData SQL database (https://population.un.org/DemoData/web/)", color = "blue",
                     hjust = 1, x = 1, face = "italic", size = 10),
  left = text_grob("SDG regions",
                   size = 14,
                   just = "centre",
                   hjust = 0.1,
                   vjust = NULL,
                   color = "royalblue", rot = 90),
  # right = "I'm done, thanks :-)!",
  # fig.lab = "Figure 1", fig.lab.face = "bold"
  )


###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Write output to Excel file --------------------------------
#https://stackoverflow.com/questions/54977747/r-error-in-as-vectorx-character-when-using-openxlsx-to-write-excel
library(openxlsx)

## Formatting can be applied simply through the write functions ## global options can be set to further simplify things options("openxlsx.borderStyle" = "thin") options("openxlsx.borderColour" = "#4F81BD")
## create a workbook and add a worksheet
# wb <- createWorkbook()
# addWorksheet(wb, "writeData auto-formatting")
wb = openxlsx::createWorkbook(creator = "Stephen Kisambira",
                        title = "All data points, ABR",
                        subject = "Distribution of data points by Data Type, SDG region and Country",
                        category = "Technical paper on selecting the adolescent birth rates (10-14 and 15-19 years) for monitoring and reporting on Sustainable Development Goals")

options("openxlsx.borderStyle" = "thin")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
cat("\014"); gc(verbose = getOption("verbose"), reset = TRUE); clearhistory()
###_________________________________________________________________________________

# Load an existing .xlsx file
#https://stackoverflow.com/questions/51039841/r-add-sheet-to-an-existing-excel-workbook
filename <- paste(output.dir,"ABR_SDG2022_SelectedData.xlsx", sep = "")

library(readxl)
#excel_sheets("Output Data/ABR_SDG2022_SelectedData.xlsx")
excel_sheets(filename)

wb <- openxlsx::loadWorkbook(filename)
options("openxlsx.borderStyle" = "thin")

#Add worksheet:
# # removeWorksheet(wb, "All data points, ABR")
openxlsx::addWorksheet(wb, "All data points, ABR", tabColour = "hotpink",
                       footer = c("&[Path]&[File]", NA, "&[Tab]"))

#____________________________________________________________________________________________
## headerStyles
hs1 <- createStyle(halign = "LEFT", textDecoration = "Bold", fontSize = 18)
hs2 <- createStyle(wrapText = TRUE)

addStyle(wb, "All data points, ABR", hs1, rows = 1, cols = c(1:20), gridExpand = TRUE)
addStyle(wb, "All data points, ABR", hs2, rows = 3, cols = 1:20, gridExpand = TRUE)

#____________________________________________________________________________________________
# Write title in worksheet
writeData(wb, "All data points, ABR", 
          x = "Distribution of data points by type of source and number of countries",
          startRow = 1, startCol = 1)

writeData(wb, "All data points, ABR", 
          x = "Distribution of data points by type of source and SDG region",
          startRow = 1, startCol = 9)

writeData(wb, "All data points, ABR", 
          x = "ABR (10-14 years) Distribution of data points by type of source and country",
          startRow = 1, startCol = 16)

writeData(wb, "All data points, ABR", 
          x = "ABR (15-19 years) Distribution of data points by type of source and country",
          startRow = 1, startCol = 25)
#____________________________________________________________________________________________
# vignette("formatting", package = "openxlsx")
# vignette("Introduction", package = "openxlsx")
setColWidths(wb, "All data points, ABR", cols = c(9,11,16,25), widths = 33)
setColWidths(wb, "All data points, ABR", cols = c(1,2:6,9:10,12:13,17:22,26:31), widths = 14)
#setColWidths(wb, "All data points, ABR", cols = c(12:13), widths = 8)

#____________________________________________________________________________________________
#https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
writeDataTable(wb, "All data points, ABR", dfcount,
               startRow = 3, startCol = 1, tableStyle = "TableStyleLight4")
writeDataTable(wb, "All data points, ABR", dfcountSDGregions, 
               startRow = 3, startCol = 9, tableStyle = "TableStyleLight3")
writeDataTable(wb, "All data points, ABR", df1014countDataSourceType, 
               startRow = 3, startCol = 16, tableStyle = "TableStyleLight2")
writeDataTable(wb, "All data points, ABR", df1519countDataSourceType, 
               startRow = 3, startCol = 25, tableStyle = "TableStyleLight2")

#____________________________________________________________________________________________
# #Create filename to save to:
# filename5 <- "Output Data/DataTypeDistributionBySDGregions.xlsx"

#Save workbook:
openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
rm(filename)
#____________________________________________________________________________________________
## objects()
rm(list=ls(pattern="^df|^hs|^mm|^kk|^ss|^ABR|^Fig|^wb|^wfd"))			## remove temporary files that contains "temp"

####___________________________________________________________________________________
gc(); cat("\014"); clearhistory() #rm_history_database
####___________________________________________________________________________________

####+++++++++++++++++++++   END OF FILE   ++++++++++++++++++++++++++++++++++++++++++++++++
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++













