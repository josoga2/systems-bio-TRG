# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the sliding_slope() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 3319-3371.

sliding_slope <- function(df, window_size = 4, baseline = 0.2, max_rate = 2) {
  
  df$wellName <- suppressWarnings(ifelse(is.nan(log(df$wellName - baseline)), 0, log(df$wellName - baseline)))
  
  df <- df[apply(df, 1, function(row) all(!is.na(row) & row != 0 & is.finite(row))), ]
  #print(df)
  max_slope <- c()
  
  
  n <- nrow(df)
  if(n < window_size){ 
    max_slope <- 0
    #stop("Data frame has fewer rows than the window size")
  }else{
    
    slopes <- numeric(n - window_size + 1)
    
    # Loop over each possible window
    for(i in 1:(n - window_size + 1)) {
      window_data <- df[i:(i + window_size - 1), ]
      # Fit linear model: growth ~ time in this window
      fit <- lm(wellName ~ Time, data = window_data)
      slopes[i] <- coef(fit)[2]  # the slope coefficient
    }
    
    # Determine the maximum computed slope
    max_slope <- max(slopes, na.rm = TRUE)
    #print(max_slope)
    
    
    # Find actual maximum growth rate
    if(max_slope <=0){
      max_slope <- 0
    }else if(max_slope < 1.7){
      max_slope <- max_slope
    }else if((max_slope > 1.7) & (max_slope <= max_rate)) {
      max_slope <- max_slope
    }else if((max_slope > max_rate)  & (max_slope < 2.2)){
      inBtw <- slopes[slopes>max_rate & slopes<=2.2]
      max_slope <- min(inBtw)
    }else if(max_slope > 2.2){
      inBtw <- slopes[slopes<max_rate]
      #print(inBtw)
      max_slope <- max(inBtw, na.rm = T)
    }
    
  }
  
  
  
  
  return(max_slope)
}
