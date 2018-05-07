remove.hru.records <- function(hrudf, zrange=200, th_area=100){

  #Description:
  # Merge HRUs with very small areas into neighbouting HRUs
  
  #Arguments:
  #hrudf   - HRU data frame object
  #zrange  - band elevation range
  #th_area - minimum area threshold
  
  #Details:
  #Find HRU entries with very small areas (i.e. area < th_area) and merge with
  #neighbouring HRU records in the same cell. The merging is done in the following
  #order of piority:
  # 1 - merge with HRU in same band
  # 2 - merge with HRU in lower band
  # 3 - merge with HRU in upper band
  # 4 - merge with HRU in upper band (when current band is sea level)
  #
  #THe target HRU is simply identified as the HRU with the smallest data frame
  #record number in either the current, lower or upper elevation band within the
  #same cell. The merging is accomplished by keeping all the values of the target
  #HRU, except area, which is updated as the sum of the area of both HRUs.
  

  area <- tmp$AREA
  records <- which(tmp$AREA < th_area)

  #Loop through filtered records
  for(r in records){
    ind_cur  <- which(tmp$CELL_ID==tmp$CELL_ID[r] & tmp$BAND_ID==tmp$BAND_ID[r])
    ind_lwr  <- which(tmp$CELL_ID==tmp$CELL_ID[r] & tmp$BAND_ID==(tmp$BAND_ID[r]-zrange))
    ind_upr2 <- which(tmp$CELL_ID==tmp$CELL_ID[r] & tmp$BAND_ID==(tmp$BAND_ID[r]+zrange))
    ind_upr1 <- which(tmp$CELL_ID==tmp$CELL_ID[r] & tmp$BAND_ID==(tmp$BAND_ID[r]+zrange/2))

    #Check if HRUs exist in current band
    if(length(ind_cur) > 1){
      ind <- min(ind_cur[which(ind_cur != r)])
      area[ind] <- tmp$AREA[ind] + tmp$AREA[r]
    #... then Check if HRUs exist in next lower band
    } else if (length(ind_lwr) > 0){
      ind <- min(ind_lwr[which(ind_lwr != r)])
      area[ind] <- tmp$AREA[ind] + tmp$AREA[r]
    #... then Check if HRUs exist in next upper band
    } else if (length(ind_upr2) > 0){
      ind <- min(ind_upr2[which(ind_upr2 != r)])
      area[ind] <- tmp$AREA[ind] + tmp$AREA[r]
    #... then Check if HRUs exist in next upper band (if current band is at sea level)
    } else if (length(ind_upr1) > 0){
      ind <- min(ind_upr1[which(ind_upr1 != r)])
      area[ind] <- tmp$AREA[ind] + tmp$AREA[r]
    } else stop(cat("Error with record '", r, "'\n", sep=""))
  }
  tmp$AREA <- area
  return(tmp[-records,])
}
