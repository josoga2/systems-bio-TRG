# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plotSampleFromFile() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1528-1562.

plotSampleFromFile <- function(abx, well, title = "Growth Curve", lp, repNum) {
  
  repNum <- repNum+1 #untreated is zero, treatments are 1,2,3
  
  mostDirectory <- "/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data/"
  abxDir <- c()
  if (abx == 'Amik') {
    abxDir <- "Full_Amikacin/"
  }
  if (abx == 'Amox') {
    abxDir <- "Full_Amoxicillin/"
  }
  if (abx == 'Cipro') {
    abxDir <- "Full_Ciprofloxacin/"
  }
  
  mostDirectory <- paste0(mostDirectory, abxDir, "lp", lp)
  
  
  All_files = list.files(mostDirectory)
  Plates = All_files[grep("txt",All_files)]
  
  mostDirectory <- paste0(mostDirectory, "/", Plates[repNum])
  #print(mostDirectory)
  
  tempData <- read.table(mostDirectory, sep = "\t", header = T)
  #print(tempData)
  with(tempData, 
       plot(x = tempData[["Time"]], 
            y = tempData[[well]], main = title,
            type = "l", ylim = c(0.05,1.4),
            lwd = 4, xlim = c(1.1,20),cex.lab = 1.5, cex.axis = 1.5,cex.main = 1.5,
            xlab = "Time (hrs)", ylab = "OD600",
            col = "#d69b43"))
}
