# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plotSampleFromFile.M9() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1564-1599.

plotSampleFromFile.M9 <- function(abx, well, title = "Growth Curve", lp, repNum) {
  
  repNum <- repNum+1 #untreated is zero, treatments are 1,2,3
  
  mostDirectory <- "/Users/josoga2/Documents/wale_docs/phd/data/M9_Project/Processed/"
  abxDir <- c()
  if (abx == 'Amik') {
    abxDir <- "Amikacin/"
  }
  if (abx == 'Amox') {
    abxDir <- "Amoxicillin/"
  }
  if (abx == 'Cipro') {
    abxDir <- "Ciprofloxacin/"
  }
  
  mostDirectory <- paste0(mostDirectory, abxDir, "lp", lp)
  print(mostDirectory)
  
  
  All_files = list.files(mostDirectory)
  Plates = All_files[grep("txt",All_files)]
  
  mostDirectory <- paste0(mostDirectory, "/", Plates[repNum])
  #print(mostDirectory)
  
  tempData <- read.table(mostDirectory, sep = "\t", header = T)
  print(tempData)
  with(tempData, 
       plot(x = tempData[["Time"]], 
            y = tempData[[well]], main = title,
            type = "l", ylim = c(0.05,0.6),
            lwd = 4, xlim = c(1.1,24),cex.lab = 1.5, cex.axis = 1.5,cex.main = 1.5,
            xlab = "Time (hrs)", ylab = "OD600",
            col = "#d69b43"))
}
