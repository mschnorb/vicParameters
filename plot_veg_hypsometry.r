plot_veg_hypsometry <- function(hru_df,
                                by_cell=FALSE,
                                by_basin=FALSE){

  #Plot area of each vegetation class by elevation band. Plot either for entire
  #study area, by grid cell or by basin (if hru_df contains almalgamated data)
  
  require("ggplot2")
  require("plyr")
  
  # Internal function(s) ###
  find.cell.area <- function(cellid,
                             area_df){
    ii <- which(area_df$CELL_ID == cellid)
    return(area_df$CELL_AREA[ii])
  }
  #######################
  
  #Set legend properties
  veg_class <- sort(unique(hru_df$CLASS))
  leg <- set_veg_legend(veg_class)
  
  no_cells <- length(unique(hru_df$CELL_ID))
  
  #Calculate HRU area fractions if not already included in hru_df
  if(is.na(match("AREA_FRAC", names(hru_df)))){
    cell_area_df <- ddply(hru_df, .(CELL_ID), summarise, CELL_AREA=sum(AREA))
    area_vector <- sapply(hru_df$CELL_ID, find.cell.area, cell_area_df)
    hru_df$AREA_FRAC <- hru_df$AREA/area_vector
  }
  
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
      scale_fill_manual(labels = leg$lbl, values=leg$clr) +
      theme_bw() + 
      labs(x="Elevation (m)", y="Area Fraction", fill="Land Cover", 
           title="Vegetation Class Hypsometry") +
      facet_wrap(~CELL_ID) +
      coord_flip()
  
  } else if(by_basin){
      gplot <- 
        ggplot(temp, aes(x=BAND_ID, y=AREA_FRAC)) + 
        geom_bar(aes(fill=factor(CLASS)), stat="identity") +
        scale_fill_manual(labels = leg$lbl, values=leg$clr) +
        theme_bw() + 
        labs(x="Elevation (m)", y="Area Fraction", fill="Land Cover") +
        facet_wrap(~basin) +
        coord_flip()

  } else {

    gplot <- 
      ggplot(temp, aes(x=BAND_ID, y=AREA_FRAC)) + 
      geom_bar(aes(fill=factor(CLASS)), stat="identity") +
      scale_fill_manual(labels = leg$lbl, values=leg$clr) +
      theme_bw() + 
      scale_x_continuous(limits=c(NA,NA)) + 
      labs(x="Elevation (m)", y="Area Fraction", fill="Land Cover", 
           title="Vegetation Class Hypsometry") +
      coord_flip()    
  }
  
  return(gplot)
}