# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the import_and_TRG() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2415-2466.

import_and_TRG <- function(whereIsFileLocated, saveFileAsPdf = T, pdfFileName = 'plateTRGView.pdf', stab = T, medium = "LB") {
  allFiles <- list.files(whereIsFileLocated)
  Plates = allFiles[grep("txt",allFiles)]
  
  if (medium == "LB") {
    MIN_START =  -4.5
    ADJ_P = 0.1
  }else {
    MIN_START = -7
    ADJ_P = 0.1
  }
  
  allPlatesSolnData = NULL
  
  if (saveFileAsPdf == T) {
    pdf(file = paste0(whereIsFileLocated, pdfFileName) ,useDingbats = F, paper = 'a4r', width = 120, height = 80)
    
    par(mfrow = c(8,12), cex=0.25, mar=c(2,2,2,2), oma=c(2,2,2,2), no.readonly = T)
    
    for (plate in Plates) {
      print(plate)
      plateData <- read.delim(file = paste0(whereIsFileLocated, plate), header = T)
      
      plateIndex <- which(plate == Plates)
      plateDate <- str_flatten(str_split(whereIsFileLocated, "[[_, /]]")[[1]][1:3])
      plateName <- plate
      
      trg_soln <- NULL
      
      
      if (stab == T) {
        trg_soln <- TRG_CALCULATOR_THREE_stab(plateData, date = plateDate, plate_no = plateIndex, od_transf_start = MIN_START, adjP = ADJ_P)
      }else{
        trg_soln <- TRG_CALCULATOR_THREE(plateData, date = plateDate, plate_no = plateIndex, od_transf_start = MIN_START)
      }
      
      
      #PLOTTING MACHINE SPECIFIC
      trg_soln$plate_no <- plateIndex
      trg_soln$plate_date <- plateDate
      trg_soln$plateName <- plateName
      
      allPlatesSolnData <- rbind(allPlatesSolnData, trg_soln)
      
      
    }
    dev.off()
    print('done')
    return(allPlatesSolnData)
  }
  
}
