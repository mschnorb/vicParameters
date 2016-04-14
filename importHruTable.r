importHruTable <- function(inPath){

  hruTable <- read.csv(inPath, stringsAsFactors=FALSE)
  hruTable <- hruTable[-1:-2]
  names(hruTable) <- c("CELL_ID", "BAND_ID", "POLY_ID", "CLASS", "BAND_AREA", "ELEVATION", "AREA")
  
  return(hruTable)
  
}