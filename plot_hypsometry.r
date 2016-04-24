plot_hypsometry <- function(hru_df, by_cell=FALSE){
  
  #Plot study area hypsometry (cumulative basin area by elevation)
  
  require("ggplot2")
  require("plyr")
  
  # Internal function(s) ###
  find.cell.area <- function(cellid,
                             area_df){
    ii <- which(area_df$CELL_ID == cellid)
    return(area_df$CELL_AREA[ii])
  }
  #######################
  
  #Calculate HRU area fractions if not already included in hru_df
  if(is.na(match("AREA_FRAC", names(hru_df)))){
    cell_area_df <- ddply(hru_df, .(CELL_ID), summarise, CELL_AREA=sum(AREA))
    area_vector <- sapply(hru_df$CELL_ID, find.cell.area, cell_area_df)
    hru_df$AREA_FRAC <- hru_df$AREA/area_vector
  }
  
  #Order data and calculate cumulative area fraction
  no_cells <- length(unique(hru_df$CELL_ID))
  if (!by_cell){
    hru_df$AREA_FRAC <- hru_df$AREA_FRAC/no_cells
    ii <- order(hru_df$ELEVATION, hru_df$AREA_FRAC)
    temp <- hru_df[ii,]
    temp$CAREA <- cumsum(temp$AREA_FRAC)
  } else {
    ii <- order(hru_df$CELL_ID, hru_df$ELEVATION, hru_df$AREA_FRAC)
    temp <- hru_df[ii,]
    csum <- ddply(temp, .(CELL_ID), summarise, cumsum(AREA_FRAC))[,2]
    temp$CAREA <- csum
  }
  
  #Create plot
  if (by_cell){
    gplot <- 
      ggplot(temp, aes(x=CAREA, y=ELEVATION)) +
      geom_line() +
      facet_wrap(~CELL_ID) +
      labs(x="Area Fraction", y="Elevation", title="Study Area Hypsometry", color="Cell ID") +
      theme_bw()
  } else {
    gplot <- 
      ggplot(temp, aes(x=CAREA, y=ELEVATION)) +
      geom_line() +
      labs(x="Area Fraction", y="Elevation", title="Study Area Hypsometry") +
      theme_bw()
  }
  
  return(gplot)
}