# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the rawDataMaker() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1101-1128.

rawDataMaker <- function(whereIsFileLocated) {
  allFiles <- list.files(whereIsFileLocated)
  
  allPlates <- c()
  
  for (file in allFiles) {
    currFolder <- paste0(whereIsFileLocated, "/", file, "/")
    
    currFiles <- list.files(currFolder)
    plates <- currFiles[grep("txt",currFiles)]
    
    currPlates <- c()
    
    for (plate in plates) {
      indPl <- read.delim(file = paste0(currFolder, plate), header = T)
      currPlates <- c(currPlates, list(indPl))
      
    }
    names(currPlates) <- c("UNT", 'Rep1', "Rep2", "Rep3")
    
    allPlates <- c(allPlates, list(currPlates))
    print(length(allPlates))
  }
  
  names(allPlates) <- lpNum
  return(allPlates)
  
}
