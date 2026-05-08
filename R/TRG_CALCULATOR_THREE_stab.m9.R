# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the TRG_CALCULATOR_THREE_stab.m9() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2840-3102.

TRG_CALCULATOR_THREE_stab.m9 <- function(plate, windowSize=4, date="", plate_no="", method, colorCode='chocolate', adjP = 0.1, min_start = -4.5) {
  
  
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
    baseline_dataframe <- data.frame('wellName' = plate[,column][2:nrow(plate)], 'Time' = plate[,'Time'][2:nrow(plate)])
    baseline_dataframe <- stabilizeR(baseline_dataframe, stabByMean = F)
    baseline_range <- derivative(baseline_dataframe, adjParam = adjP)[1]
    #print(baseline_range)
    if (baseline_range>7) {
      ranger <- c(baseline_range-1, baseline_range-2, baseline_range-3)
    }else if (baseline_range>5) {
      ranger <- c(baseline_range-1, baseline_range-2, baseline_range-3)
    }else if (baseline_range<4) {
      ranger <- c(baseline_range+1)
    }else{
      ranger <- c(baseline_range, baseline_range+1)
    }
    #print(ranger)
    baseline <- mean(baseline_dataframe$wellName[ranger[ranger>0]], na.rm = T)
    #print(baseline)
    # if (baseline > 0.19 & baseline <= 0.24) {
    #   baseline = baseline
    #   #print(baseline)
    # }else if (baseline > 0.24 & baseline <= 0.26) {
    #   baseline = 0.25
    # }else if (baseline > 0.26 & baseline <= 0.28) {
    #   baseline = 0.27
    # }else if (baseline > 0.28 & baseline <= 0.30) {
    #   baseline = 0.29
    # }else {
    #   baseline = baseline
    # }
    #print(baseline)
    elbowPoint <- head(baseline_dataframe$Time[baseline_range-3], n = 1) #track forward to select true position in the unedited data
    baseElbowPoint <- baseline_dataframe$wellName[baseline_range]
    #print(baseElbowPoint)
    #print(paste('elbowPoint =', elbowPoint))
    searchBase = baseline_dataframe$wellName[baseline_range]
    
    distSol <- 0
    
    #remove no growth baselines
    if (baseline_range>0) {
      baseline_range <- baseline_range
    }else {
      baseline_range <- length(baseline_dataframe[,'Time'])
    }
    
    if (baseline_range>0) {
      baseline <- baseline
    }else {
      baseline <- baseline_dataframe$wellName[length(baseline_dataframe[,'Time'])]
    }
    
    
    if (!is.na(tail(elbowPoint[1]))) {
      elbowPoint <- elbowPoint
    }else {
      elbowPoint <- xAxis[length(baseline_dataframe[,'Time'])]
    }
    
    #print(baseline_range)
    
    pickup <- which.min(abs(baseline_dataframe[,'wellName'] - baseline))
    #print(pickup)
    
    if (pickup < 5) {
      pickup = 4
    }
    
    
    cdTemp <- baseline_dataframe
    lBB <- nrow(subset(cdTemp, Time <= 3 & wellName <= 0.25))
    
    
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
    yAxis <- cd-baseline
    finalMaxOD = tail(yAxis, n=1)
    utcorData <- data.frame('x' = xData, 'y'= suppressWarnings(log(yAxis)))
    corData <- subset(utcorData, y>=log(exp(min_start)) & y<=log(0.5))
    #print(utcorData)
    
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
      #TRG Calculator USING new n= 0.01123556
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
    if (finalMaxOD <0.1) {
      verdi <- "No_Growth"
      
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
