rm(list=ls())

#========================================================================================================
# This is a testing/playground on the path toward a systematic review of flash flood risk. 
# The goal of this analysis is to characterize the existing research on global 
# flash flood vulnerability, risk, and exposure. 
#========================================================================================================
#install.packages("bibliometrix", dependencies=TRUE, repos = "http://cran.us.r-project.org")
#install.packages("RISmed", dependencies=TRUE, repos = "http://cran.us.r-project.org") 
 
library(bibliometrix)
library(stringr)
library(RISmed)
library(bib2df)
library(knitr)
library(rmdformats)

#-------------------------------------------------------------------------------------------------------
# Initial stuff cause I am lazy
#-------------------------------------------------------------------------------------------------------
setwd("~/Documents/GitHub/systematic-review-flash-floods")
#biblioshiny() # worth exploring

#-------------------------------------------------------------------------------------------------------
# Using "bibliometrix" to convert the .bib files from Web of Science into a dataframe. 
# dbsource = "isi" indicates from which database the collection has been downloaded (in this case Web of Science)
#-------------------------------------------------------------------------------------------------------

datafile_bib <- readFiles("Web_Of_Science_Downloads/savedrecs_1-500.bib",
                          "Web_Of_Science_Downloads/savedrecs_501-1000.bib",
                          "Web_Of_Science_Downloads/savedrecs_1001-1500.bib",
                          "Web_Of_Science_Downloads/savedrecs_1501_2000.bib",
                          "Web_Of_Science_Downloads/savedrecs_2001-2500.bib",
                          "Web_Of_Science_Downloads/savedrecs_2501-3000.bib",
                          "Web_Of_Science_Downloads/savedrecs_3001-3190.bib")

data_bib    <- convert2df(datafile_bib, dbsource = "isi", format = "bibtex")  
data_bib$AB <- str_to_sentence(data_bib$AB)
data_bib$AU <- str_to_title(data_bib$AU)
data_bib$TI <- str_to_sentence(data_bib$TI)
 

#-----------------------------------------------------------------------------------------------------
# First read in the ones that we initially read into covidence and rejected
#-----------------------------------------------------------------------------------------------------
Screenedfiles_irrelevant <- readFiles("~/Documents/GitHub/systematic-review-flash-floods/Covidence/Screened-Papers-No/review_58723_irrelevant_mendeley_20191023234131.ris")

Screenedfiles_irrelevant_TI <- Screenedfiles_irrelevant[grep("T1  -", Screenedfiles_irrelevant)]
Screenedfiles_irrelevant_TI <- substr(Screenedfiles_irrelevant_TI,7,nchar(Screenedfiles_irrelevant_TI))
FullMatches <- which(is.na(match(toupper(data_bib$TI),toupper(Screenedfiles_irrelevant_TI)))==FALSE)

data_bib$Screen1_Assessed <- NA
data_bib$Screen1_Assessed[FullMatches]  <- TRUE
data_bib$Screen1_Accept <- NA
data_bib$Screen1_Accept[FullMatches]  <- FALSE
data_bib$Reject <- NA
data_bib$Reject[FullMatches]  <- TRUE

#----------------------------------------------------------------------------------------------------
# First read in the ones that we initially read into covidence and accepted
#----------------------------------------------------------------------------------------------------
Screenedfiles_accept <- readFiles("/Users/hgreatrex/Documents/GitHub/systematic-review-flash-floods/Covidence/Screened-Papers-Yes-Maybe/review_58723_select_mendeley_20191003141113.ris")

Screenedfiles_accept_TI <- Screenedfiles_accept[grep("T1  -", Screenedfiles_accept)]
Screenedfiles_accept_TI <- substr(Screenedfiles_accept_TI,7,nchar(Screenedfiles_accept_TI))
FullMatchesAc <- which(is.na(match(toupper(data_bib$TI),toupper(Screenedfiles_accept_TI)))==FALSE)

data_bib$Screen1_Assessed[FullMatchesAc]  <- TRUE
data_bib$Screen1_Accept[FullMatchesAc]  <- TRUE


#----------------------------------------------------------------------------------------------------
# Now set up the screen 2 columns
#----------------------------------------------------------------------------------------------------
data_bib$Screen2_Assessed <- NA
data_bib$Screen2_Reject <- NA

test <- data_bib[1:10,]

#-------------------------------------------------------------------------------------------------------
# Base code to cycle through with input.  Need to add in the correct printouts, make lots of whitespace/prettyness
# and some more get-out clauses.
#-------------------------------------------------------------------------------------------------------

clr <- function(){cat(rep("\n", 30))}

for (i in 1:10){
   clr() 
   print(test$TI[i])
   cat(rep("\n", 3))
   print(test$AB[i])
   cat(rep("\n", 3))
   z <- readline(prompt="Disregard? [1]") 
   # just store the input in a list with the same key
   test$Screen2_Reject[i] <- z
}
  
