# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the corrDataTableDMSO() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1194-1224.

corrDataTableDMSO <- function(whereIsFileLocated) {
  
  All_files = list.files(paste0(whereIsFileLocated, collapse = NULL))
  Plates = All_files[grep("txt",All_files)]
  
  #import plates
  my_curr_plates <- c()
  
  n <- 0
  for (plate in Plates) {
    n <- n+1
    my_curr_ <- read.table(paste0(whereIsFileLocated, plate), header = T)[allDmsoWell]
    my_curr_plates <- c(my_curr_plates, list(my_curr_))
  }
  
  n <- 1
  colIDs <- c('Plate1')
  melted_mtx <- data.frame('Plate1' = melt(my_curr_plates[[1]][2:length(my_curr_plates[[1]])])$value)
  for (currNo in 2:length(my_curr_plates)) {
    n <- n+1
    colIDs  <- c(colIDs, paste0('Plate', n))
    print(colIDs)
    curr <- my_curr_plates[[currNo]]
    newCol <- melt(curr[2:length(curr)])$value
    melted_mtx[colIDs[currNo]] <- newCol
  }
  repMelts <- melted_mtx[1:4]
  #select just waht you need here
  whatIneed <- c(cor(repMelts))
  return(whatIneed)
}
