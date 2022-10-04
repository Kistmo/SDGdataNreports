

####____________________________________________________________________________
# title: "# Edit file  ABR_AllDatapoints2022.data after a visual check of files ABR1019_for_SDG2022_Date_today.pdf and ABR1519_for_SDG2022_Date_today.pdf "
# purpose: Select ASFRs (ABRs) for SDG reporting
# author: "Stephen Kisambira"
# date: "30/08/2022"
# output: html_document, chart; excel file

# House-keeping functions -------------------------------------------------
#rm(list=ls()) #Remove all objects.
###+++++++++++++++
here::set_here(); here::dr_here(); here::here()

# Set library path --------------------------------------------------------
.libPaths() 
#.libPaths("/Library/Frameworks/R.framework/Versions/4.2/Resources/library") #for Mac

source("abr-selection-functions.R")
####____________________________________________________________________________

# Directories (Working, Input, Utilities ----------------------------------
#Specified in "abr-selection-functions.R
here::here()

# Data sets ---------------------------------------------------------------

##Read in all the data downloaded using DemoData_ASFR.r:
#++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse); library(nlme)

#____________
## All data sources for ABR 10-14 and ABR 15-19 from DemoData:
AllDataSourcesDemoData.data <- readRDS(file="Input Data/AllDataSourcesDemoData.data.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)

#Filter out datapoints in AllDataSourcesDemoData.data with RecallLag > 8 years:
AllDataSourcesDemoData.data <- AllDataSourcesDemoData.data %>%
  filter(is.na(RecallLag) | RecallLag < 8)

#Glimpse at the data:
glimpse(AllDataSourcesDemoData.data)
str(AllDataSourcesDemoData.data)

#____________
## Data with largest number of data sources for series of each country:
MaxDataSourceSeries.data <- readRDS(file="Input Data/MaxDataSourceSeries.data.Rda") %>%
  arrange(LocName, RefYear) %>% as.data.frame(.)

#Filter out datapoints in MaxDataSourceSeries.data with RecallLag > 8 years:
MaxDataSourceSeries.data <- MaxDataSourceSeries.data %>%
  filter(is.na(RecallLag) | RecallLag < 8)

#____________
#Read in all ABR data from which selection was made for 2021
ABR_SDG2021.data <- readRDS(file="Input Data/ABR_SDG2021.data.Rda")

#____________
# Read in ABR_AllDatapoints2022.data:
ABR_AllDatapoints2022.data <- readRDS(file="Input Data/ABR_AllDatapoints2022.data.Rda")

#++++++++++++++++
#Remove duplicate data that were not reported in 2021 SDG reporting:
ABR_AllDatapoints2022.data <- ABR_AllDatapoints2022.data %>%
  #subset(., LocName=="Afghanistan") %>%
  group_by(LocID, AgeStart, RefYear) %>%
  arrange(LocID, AgeStart, RefYear) %>%  
  filter(!(is.na(SDG2021Status) & n() > 1 & (abs(ABR - lag(ABR)) < 2))) %>%
  filter(!(is.na(SDG2021Status) & n() > 1)) %>%
  ungroup()

#++++++++++++++++
#Filter out datapoints with RecallLag > 8 years:
ABR_AllDatapoints2022.data <- ABR_AllDatapoints2022.data %>%
  filter(is.na(RecallLag) | RecallLag < 8)


temp.edited <- subset(ABR_AllDatapoints2022.data,
                      !(LocName=="Bangladesh" & AgeStart==15 & RefYear < 2016 & 
                          DataCatalogShortName == "SVRS") &     
                        !(LocName=="Bangladesh" & AgeStart==15 & RefYear == 2011 & 
                            DataCatalogShortName == "2012-2013 MICS") &                                      
                        
                    # !(LocName=="Belize" & (RefYear == 2002 | RefYear == 2003 | 
                    #         RefYear == 2005 | RefYear == 2008 | RefYear == 2011) &  
                    #     DataCatalogShortName == "2015-2016 MICS") &                                     
                    !(LocName=="Benin" & AgeStart==15 &  
                        DataCatalogShortName == "2002 Census") &                                                         
                    !(LocName=="Burkina Faso" & AgeStart==15 & RefYear == 2003 &  
                        DataCatalogShortName == "2003 WHS") &  
           
                    # !(LocName=="Burundi" & AgeStart==15 &  
                    #     DataCatalogShortName == "2008 Census") &                            

                    !(LocName=="Colombia" & AgeStart==15 & 
                        RefYear == 2005 &  DataCatalogShortName == "2005 Census") &  
                      
                    # !(LocName=="Colombia" & (RefYear == 2004 | RefYear == 2006) & 
                    #     DataCatalogShortName == "Register") &                                                                            
                    # !(LocName=="Guadeloupe"  & RefYear > 2002 &
                    #     DataCatalogShortName == "Reg UNSD & WPP2019") &

                      # !(LocName=="Honduras" & RefYear == 2008 & AgeStart==15 & 
                      #     DataCatalogShortName == "Register") &  
                      
                      # !(LocName=="Jordan" & RefYear == 2013 & AgeStart==15 & 
                      #     DataCatalogShortName == "2017-2018 DHS") &                        
                        
                      # !(LocName=="Kenya" & RefYear < 2016 & AgeStart==10 & 
                      #     DataCatalogShortName == "Register") &                        

                      !(LocName=="Kenya" & AgeStart==15 & 
                          DataCatalogShortName == "Register") &
                      !(LocName=="Kenya" & RefYear == 2009 & AgeStart==15 & 
                          DataCatalogShortName == "2009 Census") &                        

                      !(LocName=="Lao People's Dem. Republic" & AgeStart==15 & 
                          RefYear == 2000 &
                          DataCatalogShortName == "2006 MICS") & 
                      
                      !(LocName=="Lesotho" & AgeStart==15 & 
                          DataCatalogShortName == "Register") &                       
                      !(LocName=="Lesotho" & AgeStart==15 & 
                          RefYear == 2014 &  DataCatalogShortName == "2018 MICS") &
                      !(LocName=="Lesotho" & AgeStart==15 & 
                          RefYear == 2015 &  DataCatalogShortName == "2016 Census") &
                                               
                      !(LocName=="Liberia" & AgeStart==15 & RefYear == 2001 & 
                          DataCatalogShortName == "2009 MIS") &   
                                            
                      !(LocName=="Madagascar" & AgeStart==15 & (RefYear < 2008 &
                          DataCatalogShortName == "2011 MIS")) &
                      
                    !(LocName=="Maldives" & AgeStart==15 & 
                        (RefYear == 2005 & DataCatalogShortName == "2006 Census")) & 
                      
                      !(LocName=="Mali" & AgeStart==15 & 
                          (DataCatalogShortName == "2009 Census" | 
                             DataCatalogShortName == "2003 WHS")) &

                      !(LocName=="Mauritania" & AgeStart==15 & 
                          (DataCatalogShortName == "2003 WHS" | 
                          (RefYear < 2008 & DataCatalogShortName == "2011 MICS") |
                          (RefYear == 2008 & DataCatalogShortName == "2015 MICS"))) &
                                   
                      # !(LocName=="Mauritania" & AgeStart==10 & RefYear == 2002 &
                      #         DataCatalogShortName == "2003 WHS") &            
                          
                      !(LocName=="Mayotte" & AgeStart==15 &
                              DataCatalogShortName == "Register") & 
                                      
                      !(LocName=="Montserrat" & AgeStart==15 &
                            (DataCatalogShortName == "Register" | 
                               (RefYear==2001 & DataCatalogShortName == "2001 Census"))) &                             
                      !(LocName=="Mozambique" & DataCatalogShortName == "2017 Census") &  

                      !(LocName=="Myanmar" & RefYear < 2016 & AgeStart==15 & 
                          DataCatalogShortName == "Register") &  
                        
                      !(LocName=="Nepal" & AgeStart==15 & 
                          (DataCatalogShortName == "2001 Census" | 
                             DataCatalogShortName == "2011 Census")) &
                        
                      # !(LocName=="Netherlands" & AgeStart==15 & 
                      #   (RefYear == 2006 & DataCatalogShortName == "Reg UNSD & WPP2019") | 
                      #   (RefYear == 2007 & 
                      #      DataCatalogShortName == "Reg Nat Stat & WPP2019")) &
                      
                      !(LocName=="Nicaragua" & AgeStart==15 & 
                          ((RefYear == 2003 | RefYear == 2010) &
                          DataCatalogShortName == "Register")) &                         
                        
                        !(LocName=="Niger" & AgeStart==15 &
                            (RefYear > 2007 & DataCatalogShortName == "2012 DHS")) &
                        
                        !(LocName=="Niger" & AgeStart==10 & 
                            RefYear == 2007 & DataCatalogShortName == "2012 DHS") & 

                        !(LocName=="Pakistan" & AgeStart==15 & 
                            DataCatalogShortName == "Annual PDS") & 
                        
                        !(LocName=="Papua New Guinea" & AgeStart==15 & 
                            DataCatalogShortName == "2011 Census") & 
                      ####START FROM HERE                       
                      
                        !(LocName=="Peru" & AgeStart==15 & RefYear < 2017 &
                            DataCatalogShortName == "Register") & 
                        
                        !(LocName=="Philippines" & (AgeStart==10 | AgeStart==15)
                          & RefYear < 2006 & DataCatalogShortName == "Register") & 
                        
                      # !(LocName=="Réunion" & AgeStart==15 & RefYear == 2019 &
                      #     DataCatalogShortName == "Register") &
                      
                      !(LocName=="Samoa" & RefYear == 2016 & AgeStart==15 & 
                          DataCatalogShortName == "2016 Census") &   
                      
                      !(LocName=="Somalia" & 
                          DataCatalogShortName == "2011 MICS (Somaliland)" | 
                          DataCatalogShortName == "2011 MICS (Northeast)") &
                      
                      !(LocName=="South Sudan" & DataCatalogShortName == "2008 Census") &     
                  
                      !(LocName=="State of Palestine"  & (AgeStart==10 | AgeStart==15) &
                          RefYear == 2007 & DataCatalogShortName == "Register") & 
                  
                      !(LocName=="Sudan"  & AgeStart==15 & RefYear == 2005 &
                          DataCatalogShortName == "2006 HHS") & 
                  
                      !(LocName=="Sudan"  & AgeStart==15 & RefYear == 2007 &
                          DataCatalogShortName == "2008 Census") &                         
                  
                      !(LocName=="Togo"  & AgeStart==15 & RefYear == 2010 &
                          DataCatalogShortName == "2010 Census") &                          
                        
                      !(LocName=="Tonga"  & AgeStart==15 & 
                          DataCatalogShortName == "2019 MICS") &
                      
                    !(LocName=="Turkmenistan" & AgeStart==15 & RefYear == 2018 & 
                        DataCatalogShortName == "2019 MICS") & 
                      
                      # !(LocName=="Tuvalu" & AgeStart==15 & 
                      #     (DataCatalogShortName == "Register" | 
                      #        DataCatalogShortName == "2012 Census")) &
                      
                      !(LocName=="Uganda" & AgeStart==15 & RefYear == 2002 &
                          DataCatalogShortName == "2002 Census") &                     
                      
                      !(LocName=="United Arab Emirates" & AgeStart==15 & RefYear == 2020 &
                          (DataCatalogShortName == "Register")) &
                      
                      !(LocName=="Uzbekistan" & AgeStart==15 & 
                        RefYear == 2018 & DataCatalogShortName == "Reg UNSD & WPP2019") &  
                      !(LocName=="Uzbekistan" & AgeStart==15 & 
                        RefYear == 2019 & DataCatalogShortName == "Register")               
                      )

saveRDS(temp.edited, file="Input Data/ABR_AllDatapoints2022.data.edited.Rda")

#Make a copy of ABR_AllDatapoints2022.data from temp.data for charting purposes:
ABR_AllDatapoints2022.data <- temp.edited
rm(temp.edited)

gc(); cat("\014"); clearhistory() #rm_history_database

###____________________________________________________________________

# PLOT EDITED ABR_AllDatapoints2022.data ----------------------------------

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

filename2 <- paste(fig.dir,"/ABR1019_for_SDG2022Edited_",Date_today,".pdf", sep = "")
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

filename2 <- paste(fig.dir,"/ABR1519_for_SDG2022Edited_",Date_today,".pdf", sep = "")
pdf(file = filename2, height = 11, width = 16,
    title = "Adolescent birth rates among girls aged 15 to 19 years")
rm(filename2)

#Run the loop through countries
lapply(abr1519countrylist$LocName, abr1014.function)
dev.off()  # Turn off device driver (to flush output to PDF)

# rm(list=ls(pattern="gg|^df|^ABR|^abr|^AllData|^MaxData|^datas|^dd|^p|^docount|^mm|^p|^ap|^abr.c")) ## remove files

### End of file.
###___________________________________________________________________________________
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@