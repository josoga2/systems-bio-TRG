# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plateDBCreator() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 3264-3315.

plateDBCreator <- function(plateDim = 384, 
                           q_annot = rep(NA,4), 
                           n_annot = "",
                           ctrls_well = NULL,
                           ctrls_annot = NULL,
                           Plate_names = c('Plate_1'),
                           Bug = 'Ec',
                           Rep_Num = c(1)) {
  
  if (length(ctrls_well) != length(ctrls_annot)) {
    stop()
  }
  
  wellName <- c()
  Annots <- rep(NA, plateDim)
  Plate_Names <- c()
  Plate_Num <- c()
  
  #build well names
  if (plateDim == 96) {
    
    for (vaR in LETTERS[1:8]) {
      wellName <- c(wellName, paste0(vaR, 1:12))
    }
    
    Annots[seq(1,96, 1)] <- n_annot
    
  }else if (plateDim == 384){
    for (vaR in LETTERS[1:16]) {
      wellName <- c(wellName, paste0(vaR, 1:24))
    }
    
    #build annot for 384 by quadrants
    Annots[seq(1,384, 4)] <- q_annot[1]
    Annots[seq(2,384, 4)] <- q_annot[2]
    Annots[seq(3,384, 4)] <- q_annot[3]
    Annots[seq(4,384, 4)] <- q_annot[4]
  }
  
  #build map
  db <- data.frame('wellName' = wellName, 'Annots' = Annots)
  
  if (length(ctrls_well) == 0  | length(ctrls_annot) == 0) {
    db <- db
  }else{
    db$Annots[db$wellName %in% ctrls_well] <- ctrls_annot  
  }
  
  
  return(db)
  
}
