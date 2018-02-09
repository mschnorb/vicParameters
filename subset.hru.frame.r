subset.hru.frame <- function(basin, cell_map, frame){
  
  #DESCRIPTION: Subset and HRU data frame (i.e. by sub-basin) from a larger HRU data frame
  # (i.e. major basin)
  
  #ARGUMENTS:
  # basin -    character providing name of sub-basin to sub-set
  # cell_map - data frame linking each VIC cell to a sub-basin; must contain fields 'CELL_ID'
  #            and 'NAME'
  # frame -    master HRU data frame containg cells for 'basin'

  #VALUE: HRU data frame for 'basin'
  
  
  ind <- which(cell_map$NAME==basin)
  if(length(ind)==0) stop(paste("Sub-basin '", basin, "' could not be found in supplied data frame.", sep=""))
  cells <- cell_map$CELL_ID[ind]
  return(do.call(rbind, lapply(cells, function(x, df){df[which(df$CELL_ID==x),]}, frame)))
  
}