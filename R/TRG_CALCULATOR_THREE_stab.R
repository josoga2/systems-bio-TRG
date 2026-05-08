# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the TRG_CALCULATOR_THREE_stab() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2586-2837.

TRG_CALCULATOR_THREE_stab <- function(plate, windowSize=4, date="", plate_no="", method, colorCode='chocolate', adjP = 0.1, od_transf_start = -4.5) {
  
  
  xAxis = plate$Time
  wellName = c(colnames(plate[2:length(plate)]))
  plateTRG = NULL
  growthRate = NULL
  slopeDetails = NULL
  corData = NULL
  RSQ = NULL
  pbl = NULL
  distance = NULL
  elbowTime = NULL
  winDiff = NULL
  INTERCEPT = NULL
  max_OD_REPORT = c()
  max_OD_VERDICT = c()
  
  for (column in 2:ncol(plate)){
    print(colnames(plate[column]))
    cd = plate[,column]
    cdDist = plate[,column]
    
    #determine baseline
    baseline_dataframe <- data.frame('wellName' = plate[,column][1:nrow(plate)], 'Time' = plate[,'Time'][1:nrow(plate)])
    baseline_dataframe <- stabilizeR(baseline_dataframe, stabByMean = F, generatePlot = T, flatt = T, trim = 4)
    baseline_range <- derivative(baseline_dataframe, adjParam = adjP)[1]
    print(baseline_range)
    if (baseline_range>23) {
      ranger <- c((baseline_range):(baseline_range-24))
    }else if (baseline_range>20) {
      ranger <- c((baseline_range-19):(baseline_range-17))
    }else {
      ranger <- c(1:baseline_range)
    }
    #print(ranger)
    #print(mean(baseline_dataframe$wellName[ranger]))
    baseline <- mean(baseline_dataframe$wellName[ranger], na.rm = T)
    
    #print(baseline)
    #print(baseline_dataframe$wellName[ranger])
    
    #print(baseline)
    elbowPoint <- tail(baseline_dataframe$Time[baseline_range-3], n = 1) #track forward to select true position in the unedited data
    baseElbowPoint <- baseline_dataframe$wellName[baseline_range]
    #print(baseElbowPoint)
    #print(paste('elbowPoint =', elbowPoint))
    searchBase = baseline_dataframe$wellName[baseline_range]
    
    distSol <- 0
    
    #remove no growth baselines
    # if (baseline_range>0) {
    #   baseline_range <- baseline_range
    # }else {
    #   baseline_range <- length(baseline_dataframe[,'Time'])
    # }
    # 
    # if (baseline_range>0) {
    #   baseline <- baseline
    # }else {
    #   baseline <- baseline_dataframe$wellName[length(baseline_dataframe[,'Time'])]
    # }
    
    
    if (!is.na(tail(elbowPoint[1]))) {
      elbowPoint <- elbowPoint
    }else {
      elbowPoint <- xAxis[length(baseline_dataframe[,'Time'])]
    }
    
    #print(baseline_range)
    
    #pickup <- (which.min(abs(baseline_dataframe[,'wellName'] - baseline)))+1
    pickup <- baseline_range-1
    
    
    cdTemp <- baseline_dataframe
  
    #print(baseline)
    #print(colnames(plate[column]))
    plot(baseline_dataframe[,'Time'], baseline_dataframe$wellName, pch=1, col=colorCode, ylim = c(0,1.2), xlim=c(0,24), 
         main = colnames(plate[column]), cex=0.5, ylab = 'Raw', xlab = 'Time')
    
    abline(v = elbowPoint, col = 'grey')
    abline(h = baseline, col= 'blue')
    
    #print(baseline)
    
    #print(pickup)
    xData = baseline_dataframe[,"Time"][pickup:nrow(baseline_dataframe)]
    #print(cd)
    cd = baseline_dataframe[,'wellName'][pickup:nrow(baseline_dataframe)]
    #assessing baseline
    #print(xData)
    
    #print(xData)
    yAxis <- cd-baseline
    #print(yAxis)
    finalMaxOD = tail(yAxis, n=1)
    #print(finalMaxOD)
    utcorData <- data.frame('x' = xData, 'y'= suppressWarnings(log(yAxis)))
    corData <- subset(utcorData, y>=log(exp(od_transf_start)) & y<=log(0.5))
    #print(utcorData)
    #print(corData)
    
    if (nrow(corData) > 1 & finalMaxOD > 0.1) {
      #run sliding window
      slope <- runner(
        x = corData,
        k = windowSize,
        at = c(4,5),
        function(x) {
          coefficients(lm(y ~ x, data = x))[2]
        }
      )
      # save index of the max slope
      whichSlope <- c(which(slope == max(slope, na.rm = T)))[[1]] 
      # save the max Slope == growth rate
      trueMaxSlope <- slope[whichSlope][[1]]
      #print(trueMaxSlope)
      
      slopeUsed <- runner(
        x = corData,
        k = windowSize,
        at = c(4,5),
        function(x) {
          lm(y ~ x, data = x)
        }
      )
      
      #print('wale')
      #print(slopeUsed)
      #print(whichSlope)
      
      slopeDetails <- slopeUsed[,whichSlope]
      transRawD <- (slopeUsed[,whichSlope]$model)$y
      indTransRawD = which(utcorData$y %in% transRawD)+ pickup#- windowSize
      fitness <- cor(x = slopeUsed[,whichSlope]$model$y, 
                     y = slopeUsed[,whichSlope]$fitted.values, 
                     method = 'pearson', use = "complete.obs")
      #min x used in calculation
      rateMin <- min((slopeUsed[,whichSlope])$model$y)
      #max y used in calculation
      rateMax <- max((slopeUsed[,whichSlope])$model$y)
      #intercept
      intercept <- coefficients(slopeDetails)[[1]]
      #print(paste('intercept = ', intercept))
      #slope 
      slope <- coefficients(slopeDetails)[[2]]
      #print(slope)
      #TRG Calculator USING new n= 0.01123556
      TRG_absolute <- (log(0.01113222) - intercept)/slope
      
      #print(TRG_absolute)
      
      if (TRG_absolute < 0 | slope < 0 | TRG_absolute > 1000) {
        TRG_absolute <- NA
        slope <- NA
      }else{
        TRG_absolute <- TRG_absolute
        slope <- slope
      }
      
      #plot point used for fitting in raw data
      #print(transRawD)
      points(x = baseline_dataframe[,'Time'][indTransRawD] , y = baseline_dataframe[,'wellName'][indTransRawD], 
             col = 'red', pch = 19)
      
      
      wDiff <- min(baseline_dataframe[,'wellName'][indTransRawD], na.rm =T) - baseline
      
      #BASELINE DISTANCE CALCULATION
      pBaseline <- rep(baseline, baseline_range)
      qPoints <- cdDist[c(1:baseline_range)]
      #print(pBaseline)
      #print(qPoints)
      xDist <- rbind(pBaseline,qPoints)
      distSol = stats::dist(xDist, method = "manhattan")
      
      #plot fits
      plot(corData, ylim = c(-6,0), xlim = c(0,24), main =colnames(plate[column]), cex=0.75 )
      points(x = (slopeUsed[,whichSlope]$model)$x, y = (slopeUsed[,whichSlope]$model)$y, col = 'red', pch=19)
      abline(slopeDetails)
      abline(h = -1.5)
      abline(h = -4)
      text(x = 0, y = 0, labels = paste('GR: ', round(slope, 2)), cex =1, pos = 4)
      text(x = 0, y = -0.5, labels = paste('TRG: ', round(TRG_absolute, 2)), cex =1, pos = 4)
      text(x = 0, y = -1.0, labels = paste('RSQ: ', round(fitness, 7)), cex =1, pos = 4)
      text(x = 0, y = -1.5, labels = paste('dist: ', c(distSol)), 7, cex =1, pos = 4)
    }else{
      TRG_absolute <- NA
      slope <- NA
      Rsq_fit <- 0
      fitness <- 0
      plot(0,24, ylim = c(-6,0), xlim = c(0,24), main =colnames(plate[column]))
      text(x = 7.5, y = -3.0, labels = paste('No growth'), cex =1, pos = 4)
      fitness <- NA
      distSol <- NA
      elbowPoint <- NA
      wDiff <- NA
      intercept <- NA
    }
    
    verdi <- c()
    if (finalMaxOD <0.15) {
      verdi <- "No_Growth"

      TRG_absolute <- NA
      slope <- NA
      Rsq_fit <- 0
      fitness <- 0
      # plot(0,24, ylim = c(-6,0), xlim = c(0,24), main =colnames(plate[column]))
      # text(x = 7.5, y = -3.0, labels = paste('No growth'), cex =1, pos = 4)
      fitness <- NA
      distSol <- NA
      elbowPoint <- NA
      wDiff <- NA
      intercept <- NA
      
    }else {
      verdi <- "Growing"
    }
    
    plateTRG <- rbind(plateTRG, TRG_absolute)
    growthRate <- rbind(growthRate, slope)
    ID <- paste(date, plate_no, wellName, sep = '_')
    RSQ <- rbind(RSQ, fitness)
    pbl <- rbind(pbl, baseline)
    distance <- rbind(distance, distSol)
    elbowTime <- rbind(elbowTime, elbowPoint)
    winDiff <- rbind(winDiff, wDiff)
    INTERCEPT <- rbind(INTERCEPT, intercept)
    max_OD_REPORT <- rbind(max_OD_REPORT, finalMaxOD)
    max_OD_VERDICT <- rbind(max_OD_VERDICT, verdi)
  }
  
  plateReport <- data.frame('wellName' = wellName, 
                            'TRG' = plateTRG, 
                            'GRate'= growthRate,
                            'Identifier' = ID,
                            'RSQ' = RSQ,
                            'pbl' = pbl,
                            'distance' = distance,
                            'elbowTime' = elbowTime, 
                            'winDiff' = winDiff,
                            'Y_INTERCEPT' = INTERCEPT,
                            'finalMaxOD' = max_OD_REPORT,
                            "Verdict" = max_OD_VERDICT)
  
  return(plateReport)
}
