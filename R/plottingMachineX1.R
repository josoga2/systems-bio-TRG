# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plottingMachineX1() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 946-981.

plottingMachineX1 <- function(folder_Name){
  pwd <- '~/Documents/wale_docs/phd/data/' # a superfolder with folders containing a subfolder named 'Output' !important
  
  All_files = list.files(paste0(pwd, folder_Name, '/Output/', collapse = NULL))
  Plates = All_files[grep("txt",All_files)]
  
  #If error, use print(Plates) to debug
  #print(Plates)
  
  par(mfrow = c(1,1))
  plot(0,0, col='white', axes = F)
  text(x = 0, y = 0, folder_Name, cex=3)
  comprehensive_solution <- NULL
  
  for (plate in Plates) {
    newPWD <- paste0(pwd, folder_Name, '/Output/',plate)
    print(newPWD)
    plate_data <- read.table(file = newPWD, sep = '\t', header = T)
    print(paste('Plotting ', plate))
    par(mfrow = c(1,1))
    plot(0,0, col='white', axes = F)
    text(x = 0, y = 0, plate, cex=3)
    par(mfrow = c(8,12), cex=0.25, mar=c(2,2,2,2), oma=c(2,2,2,2), no.readonly = T)
    
    identifier_code = str_replace_all(folder_Name, '_', '')
    
    solution = NULL
    if (length(plate_data) == 97) {
      solution = TRG_CALCULATOR_THREE(plate_data, date = identifier_code, plate_no = which(plate == Plates))
    }
    
    comprehensive_solution <- rbind(comprehensive_solution, solution)
  }
  
  return(comprehensive_solution) 
}
