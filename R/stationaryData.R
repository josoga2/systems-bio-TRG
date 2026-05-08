# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the stationaryData() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1641-1687.

stationaryData <- function(whereIsFileLocated) {
  
  baselineAll = c()
  StationaryTimeAll = c()
  StationaryODAll = c()
  lpNumAll = c()
  colNamesAll = c()
  
  
  allDirs <- list.dirs(whereIsFileLocated)
  for (folder in allDirs[2:length(allDirs)]) {
    All_files = list.files(paste0(folder, '/', collapse = NULL))
    Plates = All_files[grep("txt",All_files)]
    
    for (iPlate in Plates) {
      plateLoc <- paste0(folder,'/',iPlate)
      testDat <- read.delim(plateLoc)
      
      for (i in 2:ncol(testDat)) {
        lpNum <- paste0('LP', which(folder == allDirs)-1)
        colName <- colnames(testDat)[i]
        litData <- data.frame('Time' = testDat$Time, wellName = testDat[[i]])
        #plot(litData)
        baseline = derivative(litData)[4]
        #abline(h = baseline)
        baselineAll = c(baselineAll, baseline)
        
        corrLitData <- data.frame('Time' = litData$Time, wellName = litData$wellName-baseline)
        plot(corrLitData, main = colName, ylim = c(0,1.0), xlim = c(0,20))
        StationaryTime <- subset(corrLitData, wellName >= max(corrLitData$wellName, na.rm = F)*0.80)$Time[1]
        StationaryOD <- subset(corrLitData, wellName >= max(corrLitData$wellName, na.rm = F)*0.80)$wellName[1]
        abline(v = StationaryTime)
        StationaryTimeAll = c(StationaryTimeAll, StationaryTime)
        StationaryODAll = c(StationaryODAll, StationaryOD)
        lpNumAll = c(lpNumAll, lpNum)
        colNamesAll = c(colNamesAll, colName)
      }
    }
  }
  stationaryDataFrame <- data.frame('lpNum' = lpNumAll,
                                    'colName' = colNamesAll,
                                    'baseline' = baselineAll, 
                                    'StationaryTime' = StationaryTimeAll, 
                                    'StationaryOD' = StationaryODAll)
  stationaryDataFrame$compoundID <- paste0(stationaryDataFrame$lpNum, '_', stationaryDataFrame$colName)
  return(stationaryDataFrame)
}
