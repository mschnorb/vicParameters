merge.hru.records <-function(hrudf, ind){
  
  tag <- paste(hrudf$POLY_ID, hrudf$CLASS, sep="_")
  
  for(r in ind){
    i1 <- which(tag==tag[r])
    i2 <- i1[which(i1 != r)]
    cell_id <- hrudf$CELL_ID[i2]
    tarea <- sum(hrudf$AREA[which(hrudf$CELL_ID==cell_id)])
    hrudf$AREA[i2] <- hrudf$AREA[r] + hrudf$AREA[i2]
    hrudf$ELEVATION[i2] <- (hrudf$AREA[r]*hrudf$ELEVATION[r] +
                           hrudf$AREA[i2]*hrudf$ELEVATION[i2])/tarea
  }
  
  return(hrudf[-ind,])
  
}