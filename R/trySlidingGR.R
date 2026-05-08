# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the trySlidingGR() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 3373-3393.

trySlidingGR <- function(df, window_size = 3, baseline = 0.2, max_rate = 2, num_tries = 10) {
  #print(baseline)
  firstSlope.max <- sliding_slope(df, window_size, baseline, max_rate)
  #print(firstSlope.max)
  trials.maxGR <- c()
  #print(firstSlope.max)
  for (try in 1:num_tries) {
    #print(try)
    if (abs(firstSlope.max - max_rate) > 0.25) {
      adj_bsl <- baseline+(seq(-0.02, 0.02, length.out = try+1)[try])
      #print(adj_bsl)
      max_slope.try <- sliding_slope(df = df, window_size, adj_bsl,  max_rate)
      #print(max_slope.try)
      trials.maxGR <- c(trials.maxGR, max_slope.try)
    }else{
      trials.maxGR <- c(trials.maxGR, firstSlope.max)
    }
  }
  
  return(max(trials.maxGR))
}
