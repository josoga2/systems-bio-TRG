# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the stabilizeR() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 535-606.

stabilizeR <- function(inputWellDF, generatePlot = F, stabByMean = T, flatt = T, trim = 3) {
  #works for all wellTypes without extra colors
  
  
  
  
  #keep original data
  inputWellDF -> originalData
  inputWellDF <- inputWellDF[-c(0:trim),]
  #try sorting
  inputWellDFSort <- sort(inputWellDF$wellName, decreasing = F)
  
  
  diffInput <- (inputWellDFSort - inputWellDF$wellName)
  
  stabilizedInput <- c()
  
  
  
  if (stabByMean == F) {
    for (i in 1:length(inputWellDFSort)) {
      if(diffInput[i] > 0.001){
        stabilizedInput <- c(stabilizedInput, inputWellDF$wellName[i])
      }else if(diffInput[i] < -0.001){
        stabilizedInput <- c(stabilizedInput, inputWellDFSort[i])
      }else{
        stabilizedInput <- c(stabilizedInput, inputWellDFSort[i])
      }
    }
  }else{
    for (i in 1:length(inputWellDFSort)) {
      
      stabilizedInput <- c(stabilizedInput, mean(c(inputWellDF$wellName[i], inputWellDFSort[i])))
      
    }
    
    for (vl in 1:length(stabilizedInput[1:3])) {
      if (stabilizedInput[vl] > 0.185) {
        stabilizedInput[vl] <- 0.185
      }
      if (stabilizedInput[vl] < 0.15){
        stabilizedInput[vl] <- 0.185
        
      }
    }
  }
  
  minTime <- min(inputWellDF$Time, na.rm=T)
  
  if (flatt) {
    padX <- seq(-5, minTime, 0.3)
    #print(padX)
    padY <- rep(min(stabilizedInput, na.rm = T), length(padX))
    #return stabilized Data
    stabilizedInputDF <- data.frame("Time" = c(padX, inputWellDF[['Time']][-1]),
                                    "wellName" = c(padY, stabilizedInput[-1]))
  }else {
    stabilizedInputDF <- data.frame("Time" = c(inputWellDF[['Time']]),
                                    "wellName" = c(stabilizedInput))
  }
  
  if (generatePlot == T) {
    plot(inputWellDF, ylim = c(0,1.2), xlim = c(-5, 24))
    #print(inputWellDF)
    points(stabilizedInputDF, col ="grey")
    #print(stabilizedInputDF)
    lines(stabilizedInputDF, col = 'red', cex = 0.75)
    
  }
  
  return(stabilizedInputDF)
}
