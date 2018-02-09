plot_cum_veg_hypsometry <- function(hru_df){
  
  #Plot cumulative area of each vegetation class by elevation band for entire file
  
  pckg <- list("ggplot", "plyr")
  do.call(require, pckg)
  
  #Set legend properties
  veg_class <- sort(unique(hru_df$CLASS))
  leg <- set_veg_legend(veg_class)
  
  #Normalize area fractions
  no_cells <- length(unique(hru_df$CELL_ID))
  hru_df$AREA <- hru_df$AREA/no_cells
  
  #Calculate cumulative area by vegetation class
  ii <- order(hru_df$CLASS, hru_df$ELEVATION)
  hru_df <- hru_df[ii,-3]
  temp_df <- ddply(hru_df, .(CLASS), cumsum) #Order df, remove POLY_ID, and take cumsum
  hru_df$CAREA <- temp_df$AREA

  #Build ggplot by layers
  gplot <- ggplot(data=hru_df, aes(x=ELEVATION, y=CAREA)) + 
    geom_line(aes(color=factor(CLASS)), size=1) + 
    scale_color_manual("Land Cover", labels = leg$lbl, values=leg$clr) + 
    labs(x="Elevation (m)", y="Cumulative Area Fraction", title="Cumulative Vegetation Hypsometry") +
    coord_flip()
  
  return(gplot)
  
}