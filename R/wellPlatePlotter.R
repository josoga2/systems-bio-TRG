# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the wellPlatePlotter() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 104-146.

wellPlatePlotter <- function(SingleColData, plateType, labelTitle="", colorScale = c(0,25), gpSize = 10) {
  
  if (plateType == 96) {
    mymeta <- metafile96
    mymeta$type <- 'S1'
    mymeta$id <- 'Sample'
    mymeta$concentration <- 0.15
    rawdat <- cbind('X' = rawdata96$X , t(data.frame(matrix(data = SingleColData, nrow = 12, ncol = 8) )))
    #print(rawdat)
    rawdat <- data2plateformat(data = rawdat, platetype = 96)
    trg_df<- plate2df(rawdat)
    
    pll <- ggplot(data = trg_df, aes(x = reorder(x = row, desc(row)), y =col, fill=value))+
      geom_point(size=gpSize, pch = 21)+ylim(c(0,13))+coord_flip()+theme_bw() +
      scale_y_continuous(position = 'right' ,breaks=c(1:12), labels=c(1:12))+ 
      scale_fill_gradient2(low = 'white', mid = 'pink', midpoint = 0.5, high = 'red', limits= colorScale)+
      labs(x=NULL, y = NULL)+ theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      #geom_text(label= round(c(trg_df$value), 2), color = 'black', size=5)+
      ggtitle(label = paste0( labelTitle))
  }
  
  if (plateType == 384) {
    mymeta <- metafile384
    mymeta$type <- 'S1'
    mymeta$id <- 'Sample'
    mymeta$concentration <- 0.15
    rawdat <- cbind('X' = rawdata384$X , t(data.frame(matrix(data = SingleColData, nrow = 24, ncol = 16) )))
    #print(rawdat)
    rawdat <- data2plateformat(data = rawdat, platetype = 384)
    trg_df<- plate2df(rawdat)
    
    pll <- ggplot(data = trg_df, aes(x = reorder(x = row, desc(row)), y =col, fill=value, text = as.character(value)))+
      geom_point(size=gpSize, pch = 21)+ylim(c(0,13))+coord_flip()+theme_bw() +
      scale_y_continuous(position = 'right' ,breaks=c(1:24), labels=c(1:24))+ 
      scale_fill_gradient2(low = '#2a6a99', mid = '#c6dbef', midpoint = mean(colorScale), high = '#d88546', limits= colorScale)+
      labs(x=NULL, y = NULL)+ theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_text(label= round(c(trg_df$value), 2), color = 'black', size=3)+
      ggtitle(label = paste0( labelTitle))
  }
  return(pll)
}
