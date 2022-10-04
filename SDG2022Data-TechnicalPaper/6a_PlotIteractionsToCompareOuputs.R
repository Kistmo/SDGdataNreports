
####________________________________________________________________________________________
#---
# title: "ABR (10-14 and 15-19) datasets"
# purpose: Draw charts to show the selected data from three check levels, including output from data on ABR 15-19, based on calculation of annual change in ABR
# author: "Stephen Kisambira"
# date: "30/09/2022"
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

temp1.data <- read_excel("Output Data/ABR_SDG2022_SelectedData.xlsx",sheet = "SelectedDataForChecking1", range = cell_rows(c(1, NA))) %>%
  dplyr::mutate(., Iteration=1)

temp2.data <- read_excel("Output Data/ABR_SDG2022_SelectedData.xlsx",sheet = "SelectedDataForChecking2", range = cell_rows(c(1, NA))) %>%
  dplyr::mutate(., Iteration=2)

temp3.data <- read_excel("Output Data/ABR_SDG2022_SelectedData.xlsx",sheet = "ABR_SDG2022_FinalSelection", range = cell_rows(c(1, NA))) %>%
  dplyr::mutate(., Iteration = ifelse(AgeStart==15, 3, NA)) %>%
  filter(!is.na(Iteration))

temp.data <- temp1.data %>%
  rbind(., temp2.data) %>%
  rbind(., temp3.data)

rm(temp1.data, temp2.data, temp3.data)

names(temp.data)
temp.data$Iteration <- factor(temp.data$Iteration) 
levels(temp.data$Iteration)
str(temp.data)

temp.data$Iteration <- plyr::mapvalues(temp.data$Iteration, c("1","2","3"), c("First check","Second check","Third check"))

#' 
#' 
#' 
#' 
#' ChartsFinalCheck.fn <- function(docountry, RefYear, ABR, DataCatalogShortName) {
#'   docountry %>%
#' #'* Plot charts to check the data selected from the checking iterations
#'     ggplot( aes(x=RefYear, y=ABR, group=Iteration)) +
#' #https://stackoverflow.com/questions/49323202/change-line-color-in-ggplot    
#'     geom_line(aes(linetype=factor(Iteration), color = factor(Iteration)), 
#'               alpha=0.9, size=0.6) +
#'     scale_color_manual(values = c("blue",
#'                                   "green",
#'                                   "red")) +
#' #https://bookdown.dongzhuoer.com/hadley/ggplot2-book/legend-merge-split.html    
#'     ggnewscale::new_scale_colour() + 
#'     geom_point(aes(color=DataCatalogShortName), size=3.5) + 
#'     theme_bw() +
#'     #https://stackoverflow.com/questions/10861773/remove-grid-background-color-and-top-and-right-borders-from-ggplot2
#'     theme(axis.line = element_line(colour = "black"),
#'           panel.grid.major = element_blank(),
#'           panel.grid.minor = element_blank(),
#'           panel.border = element_blank(),
#'           panel.background = element_blank(),
#'           strip.background = element_rect(
#'             color="black", fill="white", size=0.8, linetype="solid")) +
#'     #https://stackoverflow.com/questions/30083002/ggplot-legend-list-is-larger-than-page  
#'     theme(
#'       legend.position="top", legend.direction = "vertical",
#'       panel.spacing = unit(0.1, "lines"), legend.margin=margin(),
#'       strip.text.x = element_text(size = 8), legend.title = element_blank(),
#'       legend.text = element_text(size=8),
#'       legend.key.size=unit(12,"point")) +
#'     guides(colour=guide_legend(nrow=5)) + 
#'     guides(fill = "none", size = "none") +    
#'     ggtitle(paste(docountry1a$LocName[1],": All data points", sep = "")) +
#'     theme(plot.title = element_text(size=12, hjust = 0.5)) +
#'     theme(axis.title.x = element_text(size=10, hjust = 0.5)) +
#'     theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
#'     theme(axis.title.y = element_text(size=10, hjust = 0.5)) +
#'     xlab("Reference Year") +
#'     #ylab("ABR 10-14") +
#'     scale_x_continuous(breaks = docountry1a$RefYear, guide = guide_axis(n.dodge = 2)) +
#'     coord_cartesian(ylim = c(0, max(docountry$ABR)*1.75), expand = TRUE)
#' }
#' 
#' ###++++++++++++++++ End of ChartsFinalCheck.fn Function ++++++++++++++++++++
#' 
#' 

###____________________________________________________________________

# PLOT DATA FROM CHECKING ITERATIONS ----------------------------------

####___________________________________________________________________________________
gc(); cat("\014"); clearhistory(); #rm_history_database
####___________________________________________________________________________________


# Libraries
library(ggrepel)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

#Select country list for use in lapply function below:
#temp.data <- readRDS(file="Input Data/temp.data.Rda")
abr1014countrylist <- temp.data %>%
  subset(AgeStart==10) %>%
  dplyr::group_by(LocID, AgeStart) %>%
  ##Remove countries with one data point;
  dplyr::count(LocID, LocName) %>% subset(., n > 1) %>%
  ungroup() %>%
  dplyr::distinct(LocID, .keep_all = TRUE) %>%
  select(., LocID, LocName) %>%
  dplyr::arrange(LocName)

abr1519countrylist <- temp.data %>%
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

filename2 <- paste(fig.dir,"/ABR1019_FinalCheckCharts_",Date_today,".pdf", sep = "")
pdf(file = filename2, height = 11, width = 16,
    title = "Adolescent birth rates among girls aged 10 to 14 and 15 to 19 years")
rm(filename2)

#abr1014countrylist1 <- Countrylist %>% subset(., LocName=="Afghanistan" | LocName=="Uganda")
#Run the loop through countries
lapply(abr1014countrylist$LocName, PlotsFinalCheck.fn)
dev.off()  # Turn off device driver (to flush output to PDF)

#	Create a PDF file where the graphs for ABR1519 will be printed for countries without corresponding ABR1014:
#'*https://stackoverflow.com/questions/15937131/print-to-pdf-file-using-grid-table-in-r-too-many-rows-to-fit-on-one-page *
graphics.off()
library(grDevices, grid, gridExtra)
# Create the output file:
Date_today <- format(Sys.time(), format="%d%b%Y",quietly = TRUE)
Sys.Date()

filename2 <- paste(fig.dir,"/ABR1519_FinalCheckCharts_",Date_today,".pdf", sep = "")
pdf(file = filename2, height = 11, width = 16,
    title = "Adolescent birth rates among girls aged 15 to 19 years")
rm(filename2)

#Run the loop through countries
lapply(abr1519countrylist$LocName, PlotsFinalCheck.fn)
dev.off()  # Turn off device driver (to flush output to PDF)

# rm(list=ls(pattern="gg|^df|^ABR|^abr|^AllData|^MaxData|^datas|^dd|^p|^docount|^mm|^p|^ap|^abr.c")) ## remove files

### End of file.
###___________________________________________________________________________________
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
