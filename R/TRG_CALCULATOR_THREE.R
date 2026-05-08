# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the TRG_CALCULATOR_THREE() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 722-932.

TRG_CALCULATOR_THREE <- function(plate, windowSize=4, date, plate_no, method, colorCode='chocolate', adjP = 0.1, od_transf_start = -4) {
  
  
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
  
  for (column in 2:ncol(plate)){
    print(colnames(plate[column]))
    cd = plate[,column]
    cdDist = plate[,column]
    
    #determine baseline
    baseline_dataframe <- data.frame('wellName' = plate[,column][3:nrow(plate)], 'Time' = plate[,'Time'][3:nrow(plate)])
    baseline_range <- derivative(baseline_dataframe, adjParam = adjP)[1]
    #print(baseline_range)
    #print(baseline_range)
    if (baseline_range>7) {
      ranger <- c(baseline_range-1, baseline_range-2, baseline_range-3)
    }else if (baseline_range>5) {
      ranger <- c(baseline_range-1, baseline_range-2, baseline_range-3)
    }else if (baseline_range==4) {
      ranger <- c(baseline_range+1)
    }else if (baseline_range<=3) {
      ranger <- c(baseline_range)
    }else{
      ranger <- c(baseline_range, baseline_range+1)
    }
    print(ranger)
    baseline <- mean(cd[ranger[ranger>0]], na.rm = T)
    elbowPoint <- head(baseline_dataframe$Time[baseline_range-3], n = 1) #track forward to select true position in the unedited data
    baseElbowPoint <- cd[baseline_range]
    #print(baseline)
    #print(paste('elbowPoint =', elbowPoint))
    searchBase = cd[baseline_range]
    
    distSol <- 0
    
    #remove no growth baselines
    if (baseline_range>0) {
      baseline_range <- baseline_range
    }else {
      baseline_range <- length(plate[,'Time'])
    }
    
    if (baseline_range>0) {
      baseline <- baseline
    }else {
      baseline <- cd[length(plate[,'Time'])]
    }
    
    
    if (!is.na(tail(elbowPoint[1]))) {
      elbowPoint <- elbowPoint
    }else {
      elbowPoint <- xAxis[length(plate[,'Time'])]
    }
    
    plot(xAxis, cd, pch=1, col=colorCode, ylim = c(0,1.2), xlim=c(0,24), 
         main = colnames(plate[column]), cex=0.5, ylab = 'Raw', xlab = 'Time')
    abline(v = elbowPoint, col = 'grey')
    abline(h = baseline, col= 'blue')
    
    #print(baseline_range)
    
    pickup <- head(baseline_range, n = 1)
    if (pickup < 1) {
      pickup = 1
    }else {
      pickup = pickup
    }
    
    
    
    cd = plate[,column][pickup:nrow(plate)]
    xData = plate[,'Time'][pickup:nrow(plate)]
    yAxis <- cd-baseline
    utcorData <- data.frame('x' = xData, 'y'= suppressWarnings(log(yAxis)))
    
    corData <- subset(utcorData, y>=log(exp(od_transf_start)) & y<=log(0.5))
    #print(corData)
    
    if (nrow(corData) > 1) {
      #run sliding window
      slope <- runner(
        x = corData,
        k = windowSize,
        at = c(3,4,5),
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
        at = c(3,4,5),
        function(x) {
          lm(y ~ x, data = x)
        }
      )
      
      #print('wale')
      #print(slopeUsed)
      #print(whichSlope)
      
      slopeDetails <- slopeUsed[,whichSlope]
      transRawD <- (slopeUsed[,whichSlope]$model)$y
      indTransRawD = which(utcorData$y %in% transRawD)+ pickup
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
      #TRG Calculator USING new n= 0.01123556 old: 0.0035
      TRG_absolute <- (log(0.01113222) - intercept)/slope
      
      #print(TRG_absolute)
      
      if (TRG_absolute < 0 | slope < 0) {
        TRG_absolute <- NA
        slope <- NA
      }else{
        TRG_absolute <- TRG_absolute
        slope <- slope
      }
      
      #plot point used for fitting in raw data
      points(x = plate[,'Time'][indTransRawD] , y = plate[,column][indTransRawD], 
             col = 'red', pch = 19)
      
      wDiff <- min(plate[,column][indTransRawD], na.rm =T) - baseline
      
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
      abline(h = log(0.03019738))
      abline(h = log(0.4))
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
    
    plateTRG <- rbind(plateTRG, TRG_absolute)
    growthRate <- rbind(growthRate, slope)
    ID <- paste(date, plate_no, wellName, sep = '_')
    RSQ <- rbind(RSQ, fitness)
    pbl <- rbind(pbl, baseline)
    distance <- rbind(distance, distSol)
    elbowTime <- rbind(elbowTime, elbowPoint)
    winDiff <- rbind(winDiff, wDiff)
    INTERCEPT <- rbind(INTERCEPT, intercept)
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
                            'Y_INTERCEPT' = INTERCEPT)
  
  return(plateReport)
}
