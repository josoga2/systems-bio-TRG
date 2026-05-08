# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the derivative() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 610-713.

derivative <- function(input, pl=F, adjParam = 0.15){#you can set pl = False to reduce the number of plots generated in the main function
  
  kl <- input$wellName  
  kl <- kl[3:length(kl)]
  
  
  #kl <- kl[kl > 0.175]
  
  
  
  minData <- min(kl, na.rm = T)
  #print(minData)
  
  input <- subset(input, wellName < minData+adjParam)
  #print(length(input$Time))
  #print(input$wellName)
  grads= c()
  slope <- runner(
    x = input,
    k = 4,
    idx =  c(1:length(input$Time)),
    function(x) {
      coefficients(lm(wellName ~ Time, data = x))[[2]]
    }
  )
  
  grads <- c(grads, slope)
  #print(grads)
  
  
  roundGrads <- round(grads, digit=3)
  #print(roundGrads)
  maxGrads = NULL
  
  if (length(roundGrads) > 3) {
    selection <- roundGrads[4:length(roundGrads)]
    maxGrads = which(roundGrads == max(selection, na.rm = T))-4
  }else{
    maxGrads = which(roundGrads == max(roundGrads, na.rm = T))-4
  }
  
  
  
  secondInput <- data.frame('Time' = input$Time, 'grads'=grads)
  #print()
  
  secondGrads= c()
  slope2 <- runner(
    x = na.omit(secondInput),
    k = 4,
    #idx =  c(1:length(secondInput$grads)),
    function(x) {
      coefficients(lm(grads ~ Time, data = x))[[2]]
    }
  )
  
  secondGrads <- c(secondGrads, slope2)
  #print(tail(secondGrads, 3))
  #6
  #print(tail(secondGrads, 35))
  #print(length(secondGrads))
  
  if (length(subset(input, wellName > minData , wellName < minData+0.2)$Time) > 10) {
    #solution <- which(secondGrads == max(tail(secondGrads[9:(length(secondGrads)-10)], -10), na.rm = T))
    solution <- which(secondGrads == max(tail(secondGrads, 3), na.rm = T))
  } else if (length(subset(input, wellName > minData , wellName < minData+0.2)$Time) > 6) {
    #solution <- which(secondGrads == max(tail(secondGrads[5:(length(secondGrads)-10)], -10), na.rm = T))
    solution <- which(secondGrads == max(tail(secondGrads, 3), na.rm = T))
  } else {
    #solution <- which(secondGrads == max(tail(secondGrads, 4), na.rm = T))#+4
    solution <- which(secondGrads == max(tail(secondGrads, 3), na.rm = T))
  }
  
  
  OD <- NULL
  ODFIX <- NULL
  
  #print(solution)
  solution <- solution[1]
  if (is.finite(solution) && length(solution) == 1) {
    OD <- input$wellName[solution]
    
    if ( solution == 3) {
      ODFIX <- input$wellName[solution-1]
    }else if(solution == 2 | solution == 1){
      ODFIX <- input$wellName[solution]
    }else{
      ODFIX <- input$wellName[solution-3]
    }
    if (pl==T) {
      plot(input$wellName, ylim = c(-0.2,0.4))
      lines(secondGrads, col='blue')
      abline(v = solution)
      abline(h= OD)
    }
  } else {
    solution = 0
    OD = 0
    ODFIX = 0
  }
  
  
  return(c(solution, OD,maxGrads[1], ODFIX))
}
