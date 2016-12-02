subset.hru.frame <- function(cells, frame){

  return(do.call(rbind, lapply(cells, function(x, df){df[which(df$CELL_ID==x),]}, frame)))
  
}