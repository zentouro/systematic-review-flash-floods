library(bibliometrix)
library(bib2df)
library(bibtex)
library(revtools)
library(RefManageR)


#setwd("~/Documents/GitHub/systematic-review-flash-floods")

# import saved recs 

#test_bib <- convert2df("data/01_Web-Of-Science/savedrecs_1-500.bib", dbsource = "wos", format = "bibtex")

# convert to bibliography type 

#write_bibliography(test_bib, "exports/test_bib.ris" , format = "ris")
#df2bib(test_bib, "exports/text_bib.bib", append = FALSE)
#WriteBib(test_bib, file = "exports/test2_bib.bib", biblatex = TRUE,append = FALSE, verbose = TRUE)
#df2bib(data_bib, "exports/text2_bib.bib", append = FALSE)

# export subsection to see if it will still open in zotero 

#DI <- data_bib$DI
#capture.output(DI, file = "exports/test.txt")



#export CSV with DOI from data_bib
#write.csv(data_bib$DI,"exports/test.csv", row.names = FALSE)
write.csv(data_bib$DI, "exports/DOIs-all.csv", row.names = data_bib$TI)





