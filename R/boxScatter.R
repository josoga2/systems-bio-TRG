# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the boxScatter() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 3120-3172.

boxScatter <- function(x, y,
                       XLIM, YLIM,
                       COL = 'black', 
                       PCH.SC = 1, 
                       cex = 0.75,
                       col.box = 'grey',
                       bg.col = NULL,
                       bg.box = NULL) {
  
  layout(mat = matrix(c(2, 1, 0, 3), 
                      nrow = 2, 
                      ncol = 2),
         heights = c(1, 6),    # Heights of the two rows
         widths = c(6, 1)) # Widths of the two columns
  
  
  #scatterplot
  par(mar = c(5,4,0,0))
  plot(x = x, 
       y = y,
       xlab = "height", 
       ylab = "weight", 
       pch = PCH.SC, 
       col = COL,
       bg = bg.col,
       xlim = XLIM,
       ylim = YLIM,
       cex = cex)
  
  # Plot 2: Top (height) boxplot
  par(mar = c(0, 4, 0, 0))
  boxplot(x, xaxt = "n",
          yaxt = "n", bty = "n",
          col = col.box, 
          frame = F, 
          horizontal = TRUE,
          ylim = NULL,
          cex = cex,
          bg = bg.box,
          pch = PCH.SC)
  
  # Plot 3: Right (weight) boxplot
  par(mar = c(5, 0, 0, 0))
  boxplot(y, 
          xaxt = "n",
          yaxt = "n", bty = "n",
          col = col.box, 
          bg = bg.box,
          frame = F,
          ylim = YLIM,
          pch = PCH.SC)
  
}
