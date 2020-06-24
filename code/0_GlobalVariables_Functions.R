#==============================================================================================
# Global variables
#==============================================================================================
main.project <- "systematic-review-flash-floods"
input.name  <- "ScreenedData-0604-ACTIVE.rData"

#==============================================================================================
# This little chunk will automatically set up the subdirectory locations for anyone's computer
# from the location of this file.  It will also laod in the screened data as named above in the
# 05_screened data subfolder.
#==============================================================================================
if(grep(main.project,address) <= 0){
   stop(paste("Warning!! Your main folder is not called", main.project,
              "!Go into the 0_GlobalVariables_Functions.R and change it"))
}else{
   print(paste("WELCOME!"),quote=FALSE)
   
   
   # MAIN FOLDER LOCATION
   folder.github <- substr(strsplit(address, main.project)[[1]][1],1,nchar(strsplit(address, main.project)[[1]][1])-1)
   
   # Now work out if you are on a mac or a PC and get the separator
   if(length(grep(folder.github,"\\")) > 0){
      separator <- "\\"
      print(paste("You are on a PC"))      
   }else{
      separator <- "/"
      print(paste("You are on a mac or linux"),quote=FALSE)   
   }   
   
   # SUB-FOLDER LOCATIONS
   folder.data <-  paste(folder.github,main.project,"data","05_screened-data",sep=separator)
   folder.code <-  paste(folder.github,main.project,"code",sep=separator)
   
   # CORE FILE LOCATIONS
   file.datain <- paste(folder.data,input.name,sep=separator)
   
   # SHAPE FILE LOCATI 
   
   # Load the data
   load(file.datain)
   print(paste("Your data is stored as the variable data_bib"),quote=FALSE)
}


#==============================================================================================
#
#==============================================================================================








