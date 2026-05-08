# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plateViewer() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2355-2387.

plateViewer <- function(whereIsFileLocated, saveFileAsPdf = T, pdfFileName = 'plateOutView.pdf', plootype = 'p') {
  allFiles <- list.files(whereIsFileLocated)
  Plates = allFiles[grep("txt",allFiles)]
  
  allPlatesData = c()
  
  if (saveFileAsPdf == T) {
    pdf(file = paste0(whereIsFileLocated, pdfFileName) ,useDingbats = F, paper = 'a4r', width = 120, height = 80)
    
    
    
    for (plate in Plates) {
      #annotate page
      par(mfrow = c(1,1))
      plot(1,1, col = 'white')
      text(1,1, plate, cex = 4)
      
      
      par(mfrow = c(8,12), cex=0.25, mar=c(2,2,2,2), oma=c(2,2,2,2), no.readonly = T)
      
      plateData <- read.delim(file = paste0(whereIsFileLocated, plate), header = T)
      allPlatesData <- c(allPlatesData, list(plateData))
      
      for (well in 2:length(colnames(plateData))) {
        plot(plateData$Time, plateData[,well], pch = 19, col ="#924154", cex=0.75, ylim = c(0,1.4),
             main = colnames(plateData)[well], xlab = "Time", ylab = "OD600", type = plootype)
      }
    }
    dev.off()
    return(allPlatesData)
  }
  
}
