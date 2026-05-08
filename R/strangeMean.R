# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the strangeMean() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1906-2074.

strangeMean <- function(antibiotics) {
  
  treatment = NULL
  if (antibiotics == 'Amik') {
    treatment = amikacinCompleteDataAll
  }else if(antibiotics == 'Amox'){
    treatment = amoxicillinCompleteDataAll
  }else if(antibiotics == 'Cipro'){
    treatment = ciproCompleteDataAll
  }else if(antibiotics == 'Cipro.M9'){
    treatment = FullCipro.M9.Zero
  }else if(antibiotics == 'Amox.M9'){
    treatment = FullAmox.M9.Zero
  }
  #print(head(treatment))
  
  #untreated
  untMeanData <- c()
  untSdData <- c()
  meanGRate.unt <- c()
  
  #combine Treat
  meanData <- c()
  fcData <- c()
  pvData <- c()
  sdData <- c()
  residData <- c()
  residOneData <- c()
  lpN <- c()
  meanGRate <- c()
  
  #subset each library compound
  libComps <- unique(treatment$compoundID.x) #fix
  #print(libComps)
  #print(head(unique(treatment$compoundID.x)))
  for (i in libComps) {
    #current lib compound
    #print(i)
    subLibComp <- subset(treatment, compoundID.x == i) #fix
    lpN <- c(lpN, unique(subLibComp$lpNum.x)) #fix
    
    #untreated and treated
    curUnt <- subLibComp$UNT.med_TRG_DIFF
    curTreat <- subLibComp$med_TRG_DIFF
    
    #growth rate
    curGRate.unt <- subLibComp$UNT.GRate
    curGRate <- subLibComp$GRate
    
    #print(curGRate.unt)
    
    #residuals
    curResid <- subLibComp$medTRGDIFFVuntResid
    curResidOne <- subLibComp$medTRGDIFFVoneResid
    
    #print(curResid)
    
    #remove all zero
    if (length(which(curUnt != 0)) == 1 | length(which(curTreat != 0)) == 1) {
      
      #mean Data
      curUnt <- curUnt[curUnt != 0]
      curTreat <- curTreat[curTreat != 0]
      untMeanData <- c(untMeanData, mean(curUnt[curUnt != 0], na.rm = T))
      meanData <- c(meanData, mean(curTreat[curTreat != 0], na.rm = T))
      
      #sd data
      sdData <- c(sdData, 0)
      untSdData <- c(untSdData, 0)
      
      #pvdata
      pvData <- c(pvData, 0)
      
      #residData
      residData <- c(residData, mean(curResid[curResid != 0], na.rm = T))
      residOneData <- c(residOneData, mean(curResidOne[curResidOne != 0], na.rm = T))
      
      #GRATE
      meanGRate.unt <- c(meanGRate.unt, mean(curGRate.unt[curGRate.unt != 0], na.rm = T))
      meanGRate <- c(meanGRate, mean(curGRate[curGRate != 0], na.rm = T))
      
      
      
    }else if (length(which(curUnt == 0)) > 1 | length(which(curTreat == 0)) > 1) {
      untMeanData <- c(untMeanData, 0)
      untSdData <- c(untSdData, 0)
      meanGRate.unt <- c(meanGRate.unt, 0)
      
      ##
      meanData <- c(meanData, 0)
      pvData <- c(pvData, 0)
      sdData <- c(sdData, 0)
      meanGRate <- c(meanGRate, 0)
      
      #aggregate residuals as well
      residData <- c(residData, mean(curResid[curResid != 0]))
      residOneData <- c(residOneData, mean(curResidOne[curResidOne != 0]))
      
      
    }else{
      curUnt <- curUnt[curUnt != 0]
      curTreat <- curTreat[curTreat != 0]
      #mean data
      untMeanData <- c(untMeanData, mean(curUnt, na.rm = T))
      meanData <- c(meanData, mean(curTreat, na.rm = T))
      
      #sd data
      untSdData <- c(untSdData, sd(curUnt, na.rm = T))
      sdData <- c(sdData, sd(curTreat, na.rm = T))
      
      #pvData
      curPV <- t.test(curUnt, curTreat)$p.value
      pvData <- c(pvData, curPV)
      
      #aggregate residuals as well
      residData <- c(residData, mean(curResid[curResid != 0], na.rm = T))
      residOneData <- c(residOneData, mean(curResidOne[curResidOne != 0], na.rm = T))
      
      #GRATE
      meanGRate.unt <- c(meanGRate.unt, mean(curGRate.unt[curGRate.unt != 0], na.rm = T))
      meanGRate <- c(meanGRate, mean(curGRate[curGRate != 0], na.rm = T))
      
     

      # curGRate.unt <- curGRate.unt[curGRate.unt != 0]
      # curGRate <- curGRate[curGRate != 0]
      # 
      # untMeanData <- c(untMeanData, mean(curGRate.unt, na.rm = T))
      # meanData <- c(meanData, mean(curGRate, na.rm = T))
      
    }
  }
  
  #fcData
  curFC <- meanData - untMeanData / untMeanData
  fcData <- c(fcData, curFC)
  
  # print(length(libComps))
  # print(length(untMeanData))
  # print(length(untSdData))
  # print(length(meanData))
  # print(length(sdData))
  # print(length(fcData))
  # print(length(pvData))
  # print(length(meanGRate))
  # print(length(meanGRate.unt))
  # print(length(antibiotics))
  # print(length(residData))
  # print(length(residOneData))
  # print(length(lpN))
  
  
  statOut <- data.frame("compoundID" = libComps,
                        "UNT.MEAN" = untMeanData,
                        "UNT.SD" = untSdData,
                        "MEAN" = meanData,
                        "SD" = sdData,
                        "fcData" = fcData,
                        "pvData" = pvData,
                        "residuals" = residData,
                        "residualToOne" = residOneData,
                        "PlateTreatment" = rep(antibiotics, 2304),
                        "lpNum" = lpN,
                        "meanGRate.unt" = meanGRate.unt,
                        "meanGRate" = meanGRate)
  
  return(statOut)
  
}
