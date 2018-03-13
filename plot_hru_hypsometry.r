plot_hru_hypsometry <- function(hru_df){
  
  #...
  
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
  
  #Order HRU data frame
  tmp <- hru_df[order(hru_df$BAND_ID, hru_df$CLASS, hru_df$AREA),]

  #Calculate HRU area fractions if not already included in hru_df
  if(is.na(match("AREA_FRAC", names(tmp)))){
    cell_area_df <- ddply(tmp, .(CELL_ID), summarise, CELL_AREA=sum(AREA))
    area_vector <- sapply(tmp$CELL_ID, find.cell.area, cell_area_df)
    tmp$AREA_FRAC <- tmp$AREA/area_vector
  }
  #Calculate cumulative area fraction and count by BAND_ID
  tmp$CAREAF <- NA
  tmp$COUNT  <- NA
  for(b in unique(tmp$BAND_ID)){
    tmp$CAREAF[which(tmp$BAND_ID==b)] <-
      cumsum(tmp$AREA_FRAC[which(tmp$BAND_ID==b)])
    tmp$COUNT[which(tmp$BAND_ID==b)] <- 1:length(which(tmp$BAND_ID==b))
  }

  gplot <- 
    #ggplot(tmp, aes(x=BAND_ID, y=COUNT)) + 
    ggplot(tmp, aes(x=BAND_ID, y=factor(CLASS))) + 
    #geom_point(aes(colour=factor(CLASS), size=AREA_FRAC), shape=1, alpha=1/2) +
    geom_count(aes(colour=factor(CLASS)), alpha=1/2) +
    scale_colour_manual(labels = leg$lbl, values=leg$clr) +
    scale_size(breaks=seq(0,5000,1000), range = c(2,20)) +
    theme_bw() + 
    labs(x="Elevation (m)", y="Vegetation Class", colour="Class", 
         size = "Count", title="HRU Hypsometry") +
    coord_flip()    

  return(gplot)
}