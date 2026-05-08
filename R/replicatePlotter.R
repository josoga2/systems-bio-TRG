# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the replicatePlotter() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1263-1309.

replicatePlotter <- function(whereIsFileLocated, well, header="", legend=F, plRep=T, repToPick = 2) {
  
  All_files = list.files(paste0(whereIsFileLocated, collapse = NULL))
  Plates = All_files[grep("txt",All_files)]
  
  myFilesLocated <- list()
  for (file in Plates) {
    myFilesLocated <- c(myFilesLocated, list(read.table(paste0(whereIsFileLocated, file), header = T)))
  } 
  
  plot(x = myFilesLocated[[1]]$Time, y = myFilesLocated[[1]][[well]], 
       xlim = c(1,24), ylim = c(0,1.3), type = 'l', xlab = 'Time', ylab = 'OD600',
       lwd = 4, main = paste0( header))
  
  #plot DMSO4 (A1) untreated
  lines(x = myFilesLocated[[1]]$Time, y = myFilesLocated[[1]][[allDmsoWell[1]]],
        lwd = 4, col = 'grey')
  
  #plot DMSO4 (A1) treated
  lines(x = myFilesLocated[[1]]$Time, y = myFilesLocated[[3]][[allDmsoWell[1]]],
        lwd = 4, col = 'chocolate')
  
  #plot the wells in replicates
  if (plRep == T) {
    for (repPla in 2:length(myFilesLocated)) {
      lines(x = myFilesLocated[[repPla]]$Time, y= myFilesLocated[[repPla]][[well]],
            lwd = 4, col = repPla)
    }
  }else{
    for (repPla in c(repToPick)) {
      lines(x = myFilesLocated[[repPla]]$Time, y= myFilesLocated[[repPla]][[well]],
            lwd = 4, col = 3)
    }
  }
  
  
  #plot the legend
  if (legend == TRUE) {
    legend('bottomright', legend = c(paste0('Untreated+',well), paste0(well, ' rep1'), 
                                     paste0(well, ' rep2'), paste0(well, ' rep3'),
                                     paste0('Untreated+DMSO'), paste0('Treated+DMSO')), 
           col = c(1:4, 'grey', 'chocolate'), 
           pch = 19, cex = 0.5)
  }
  
  
}
