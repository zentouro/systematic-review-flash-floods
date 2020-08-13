library(bib2df) 
library(revtools)


setwd("~/Documents/GitHub/systematic-review-flash-floods")
Workingfile <- "data/screeningData.RData"
load(Workingfile)
load(data_bib)



# export as a .bib

df2bib(data_bib, file = "exports/test.bib", append = FALSE)

# export as a .RIS

# write_bibliography(data_bib, "exports/test.ris", format = "ris")
