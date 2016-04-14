summarize.df <- function(x){
  d <- ddply(x, .(CLASS), summarize, AREA=sum(AREA), ELEV=median(ELEVATION))
  d$AREA_FRAC <- d$AREA/sum(d$AREA)
  
  return(d)
  
}