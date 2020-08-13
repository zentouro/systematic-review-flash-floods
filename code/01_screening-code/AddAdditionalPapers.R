#========================================================================================================
# This code add abstracts to an active rData file for systematic review.
# Originally developed for characterize the existing research on global 
# flash flood vulnerability, risk, and exposure. 
#========================================================================================================

library(bibliometrix)
library(stringr)
library(RISmed)
library(bib2df)
library(knitr)
library(rmdformats)

#-----------------------------------------------------------------------------------
# Initializing Stuff
#-----------------------------------------------------------------------------------

#adjust this to the appropriate folder on your system
setwd("~/Documents/GitHub/systematic-review-flash-floods")

#-----------------------------------------------------------------------------------
# Load in screeningData file to include new papers
#-----------------------------------------------------------------------------------
load("data/screeningData.RData")
previous_data_bib <- data_bib 

#-----------------------------------------------------------------------------------
# Using "bibliometrix" to convert the .bib files from Web of Science into a dataframe. 
# dbsource = "isi" indicates from which database the collection has been downloaded (in this case Web of Science)
#-----------------------------------------------------------------------------------

datafile_bib <- readFiles("data/01_Web-Of-Science/May13-update/savedrecs1-500.bib",
                          "data/01_Web-Of-Science/May13-update/savedrecs501-557.bib")


data_bib <- convert2df(datafile_bib, dbsource = "isi", format = "bibtex")
data_bib$AB <- str_to_sentence(data_bib$AB)
data_bib$AU <- str_to_title(data_bib$AU)
data_bib$TI <- str_to_sentence(data_bib$TI)

#-----------------------------------------------------------------------------------
# Adding Screen 1 collumns to match
#-----------------------------------------------------------------------------------

data_bib$Screen1_Assessed <- NA
data_bib$Screen1_Accept <- NA
data_bib$Reject <- NA

#-----------------------------------------------------------------------------------
# Now set up the screen 2 columns
#   including: assessed or rejected, and columns for the sorting, tags 
#   we are interested in
#-----------------------------------------------------------------------------------

data_bib$Screen2_Assessed <- NA
data_bib$Screen2_Reject <- NA


data_bib$AB <- as.character(data_bib$AB)
data_bib$Shiny_Screen <- NA


data_bib$Screen2_Event <- NA
data_bib$Screen2_Precip <- NA
data_bib$Screen2_Model <- NA
data_bib$Screen2_Social <- NA
data_bib$Screen2_FlashFloodDatabase <- NA
data_bib$Screen2_typeID <- NA
data_bib$Screen2_Notes <- ""

# test <- data_bib[1:10,]

#-----------------------------------------------------------------------------------
# Update previous file with new references
#-----------------------------------------------------------------------------------

updated_data_bib <- rbind(previous_data_bib, data_bib)
data_bib <- updated_data_bib

#-----------------------------------------------------------------------------------
# Save updated screening data file
#-----------------------------------------------------------------------------------

save(data_bib, file = "data/screeningData-updated.rData")





