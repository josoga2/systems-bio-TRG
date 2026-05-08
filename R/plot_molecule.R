# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plot_molecule() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1731-1754.

plot_molecule <- function(molecule, name = NULL, sma = NULL, ...){
  #' molecule an object as returned by rcdk::load.molecules or rcdk::parse.smiles()
  #' name a character for the name of the molecule, 
  #' sma a character witht the smarts string as passed onto get.depictor()
  #' ... other arguments for get.depictor()
  
  # Image aesthetics 
  dep <- get.depictor(
    width = 1000, height = 1000,
    zoom = 7, sma = sma, ...
  )
  molecule_sdf <- view.image.2d(molecule[[1]], depictor = dep)
  
  
  ## Remove extra margins around the molecule
  par(mar=c(0,0,0,0))
  plot(NA, 
       xlim=c(1, 10), ylim=c(1, 10), 
       # Remove the black bounding boxes around the molecule
       axes = F)
  rasterImage(molecule_sdf, 1,1, 10,10)
  # Annotate the molecule
  text(x = 5.5, y = 1.1,  deparse(substitute(molecule)))
}
