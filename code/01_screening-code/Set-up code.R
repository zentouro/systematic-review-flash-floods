rm(list=ls())

#========================================================================================================
# This code creates a rData file of abstracts for systematic review.
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
# Using "bibliometrix" to convert the .bib files from Web of Science into a dataframe. 
# dbsource = "isi" indicates from which database the collection has been downloaded (in this case Web of Science)
#-----------------------------------------------------------------------------------

datafile_bib <- readFiles("data/01_Web-Of-Science/savedrecs_1-500.bib",
                          "data/01_Web-Of-Science/savedrecs_501-1000.bib",
                          "data/01_Web-Of-Science/savedrecs_1001-1500.bib",
                          "data/01_Web-Of-Science/savedrecs_1501_2000.bib",
                          "data/01_Web-Of-Science/savedrecs_2001-2500.bib",
                          "data/01_Web-Of-Science/savedrecs_2501-3000.bib",
                          "data/01_Web-Of-Science/savedrecs_3001-3190.bib")

data_bib    <- convert2df(datafile_bib, dbsource = "isi", format = "bibtex")  
data_bib$AB <- str_to_sentence(data_bib$AB)
data_bib$AU <- str_to_title(data_bib$AU)
data_bib$TI <- str_to_sentence(data_bib$TI)
 

#-----------------------------------------------------------------------------------
# First read in the ones that we initially read into covidence and rejected
#-----------------------------------------------------------------------------------
Screenedfiles_irrelevant <- readFiles("~/Documents/GitHub/systematic-review-flash-floods/data/02_Covidence/Screened-Papers-No/review_58723_irrelevant_mendeley_20191023234131.ris")

Screenedfiles_irrelevant_TI <- Screenedfiles_irrelevant[grep("T1  -", Screenedfiles_irrelevant)]
Screenedfiles_irrelevant_TI <- substr(Screenedfiles_irrelevant_TI,7,nchar(Screenedfiles_irrelevant_TI))
FullMatches <- which(is.na(match(toupper(data_bib$TI),toupper(Screenedfiles_irrelevant_TI)))==FALSE)

data_bib$Screen1_Assessed <- NA
data_bib$Screen1_Assessed[FullMatches]  <- TRUE
data_bib$Screen1_Accept <- NA
data_bib$Screen1_Accept[FullMatches]  <- FALSE
data_bib$Reject <- NA
data_bib$Reject[FullMatches]  <- TRUE

#-----------------------------------------------------------------------------------
# Read in the abstracts that we initially read into covidence and accepted
#-----------------------------------------------------------------------------------
Screenedfiles_accept <- readFiles("~/Documents/GitHub/systematic-review-flash-floods/data/02_Covidence/Screened-Papers-Yes-Maybe/review_58723_select_mendeley_20191003141113.ris")

Screenedfiles_accept_TI <- Screenedfiles_accept[grep("T1  -", Screenedfiles_accept)]
Screenedfiles_accept_TI <- substr(Screenedfiles_accept_TI,7,nchar(Screenedfiles_accept_TI))
FullMatchesAc <- which(is.na(match(toupper(data_bib$TI),toupper(Screenedfiles_accept_TI)))==FALSE)

data_bib$Screen1_Assessed[FullMatchesAc]  <- TRUE
data_bib$Screen1_Accept[FullMatchesAc]  <- TRUE


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

test <- data_bib[1:10,]


save(data_bib, file = "data/screeningData-updated.rData")


#-----------------------------------------------------------------------------------
# Now set up the screen 3 columns
#     screening the social code 
#-----------------------------------------------------------------------------------
setwd("~/Documents/GitHub/systematic-review-flash-floods")
Workingfile <- "data/screeningData.RData"
load(Workingfile)

bib_all       <- data_bib; rm(data_bib)
data_bib      <- bib_all[which(bib_all$Screen2_Social == TRUE),]

data_bib$Screen3_Assessed <- NA
data_bib$Screen3_Reject   <- NA

data_bib$Screen3_meta       <- ""     
data_bib$Screen3_assessment <- ""
data_bib$Screen3_before     <- ""   
data_bib$Screen3_during     <- ""
data_bib$Screen3_impact     <- ""
data_bib$Screen3_methods    <- ""
data_bib$Screen3_geo        <- ""
data_bib$Screen3_flood      <- ""
data_bib$Screen3_Notes      <- ""

save(data_bib, file = "data/screeningSocialData.rData")
