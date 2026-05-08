# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the quadrSave_View() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2389-2411.

quadrSave_View <- function(whereIsFileLocated) {
  
  source(paste0("/Users/josoga2/Documents/wale_docs/phd/R/Collection_of_scripts","/Aux_functions.R",collapse=NULL))
  
  allFiles <- list.files(whereIsFileLocated)
  Plates = allFiles[grep("txt",allFiles)]
  
  SplitFolder <- paste0(whereIsFileLocated, '/Quadrant_Split/')
  
  dir.create(SplitFolder, recursive = T)
  
  for (i in Plates) {
    for (k in 1:4) {
      write.table(x = quadr(quad = k, dataset = read.delim( paste0(whereIsFileLocated, i), header = T)), 
                  file = paste0(SplitFolder, '/', i, '_Q_', k, '.txt'), quote = F, row.names = F, sep = '\t')
    }
  }
  
  print(SplitFolder)
  plateViewer(SplitFolder)
  
  
}
