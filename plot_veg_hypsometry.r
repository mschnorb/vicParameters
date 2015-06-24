plot_veg_hypsometry <- function(hru_df, by_cell=FALSE, by_basin=FALSE){

  #Plot area of each vegetation class by elevation band. Plot either for entire
  #study area, by grid cell or by basin (if hru_df contains almalgamated data)
  
  require("ggplot2")
  
  no_cells <- length(unique(hru_df$CELL_ID))
  
  if (!by_cell) hru_df$AREA_FRAC <- hru_df$AREA_FRAC/no_cells
  
  #Normalize area by basin
  if (by_basin){
    hru_df$AREA_FRAC <- hru_df$AREA_FRAC*no_cells
    basins <- unique(hru_df$basin)
    for (b in basins){
      i <- which(hru_df$basin==b)
      no_cells_b <- length(unique(hru_df$CELL_ID[i]))
      hru_df$AREA_FRAC[i] <- hru_df$AREA_FRAC[i]/no_cells_b
    }
  }
  
  ii <- order(hru_df$CLASS, hru_df$ELEVATION, hru_df$CELL_ID)
  temp <- hru_df[ii,]
  
  if (by_cell){
      gplot <- 
      ggplot(temp, aes(x=BAND_ID, y=AREA_FRAC)) + 
      geom_bar(aes(fill=factor(CLASS)), stat="identity") +
      theme_bw() + 
      labs(x="Elevation (m)", y="Glacier Area Fraction", color="Cell ID", fill="Land Cover", 
           title="Vegetation Class Hypsometry") +
      facet_wrap(~CELL_ID) +
      coord_flip()
  
  } else if(by_basin){
      gplot <- 
        ggplot(temp, aes(x=BAND_ID, y=AREA_FRAC)) + 
        geom_bar(aes(fill=factor(CLASS)), stat="identity") +
        #scale_fill_brewer(type="qual", palette="Paired") +
        scale_fill_manual(labels = c("Alpine Conifer", "High-elevation Conifer", "Mid-elevation Conifer",
                    "Humid Conifer", "Temperate Deciduous", "Shrub", "Grassland","Grass-lichen-moss", 
                    "Wetland", "Barren", "Water", "Ice"),
              values=c("#CCFF66", "#99FF00", "#33FF00", "#336600", "#CCFF99", "#FFCC99",
                  "#CCCC33", "#CCCC99", "#009966", "#996633", "#3399FF", "#99CCFF")) +
        theme_bw() + 
        labs(x="Elevation (m)", y="Area Fraction", color="Cell ID", fill="Land Cover") +
        facet_wrap(~basin) +
        coord_flip()

  } else {
    gplot <- 
      ggplot(temp, aes(x=BAND_ID, y=AREA_FRAC)) + 
      geom_bar(aes(fill=factor(CLASS)), stat="identity") +
      theme_bw() + 
      labs(x="Elevation (m)", y="Area Fraction", color="Cell ID", fill="Land Cover", 
           title="Vegetation Class Hypsometry") +
      coord_flip()    
  }
  
  return(gplot)
}