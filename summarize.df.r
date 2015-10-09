summarize.df <- function(x){
  d <- ddply(x, .(CLASS), summarize, AREA_FRAC=sum(AREA_FRAC))
  d$ELEVATION <- sum(x$ELEVATION*x$AREA_FRAC)/sum(x$AREA_FRAC)
  d$BAND_ID <- d$ELEVATION
  
  return(d)
  
}