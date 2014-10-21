plot_veg_hypsometry <- function(hru_df){

  require("ggplot2")
  
  #temp <- ddply(bridge_hru, .(CLASS, CELL_ID, ELEVATION))
  ii <- order(hru_df$CLASS, hru_df$ELEVATION, hru_df$CELL_ID)
  temp <- hru_df[ii,]
  
  gplot <- 
    ggplot(temp, aes(x=BAND_ID, y=AREA_FRAC)) + 
    geom_bar(aes(y=AREA_FRAC, fill=factor(CLASS)), stat="identity") +
    theme_bw() + 
    labs(x="Elevation (m)", y="Area Fraction", color="Cell ID", fill="Land Cover", 
         title="Vegetation Class Hypsometry") +
    coord_flip()
  
  return(gplot)
}