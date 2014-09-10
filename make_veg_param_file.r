make_veg_param_file <- function(hru_df){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #DESCRIPTION
  # Create vegetation parameter file for VIC model version 4.1.2glacier; vegetation parameter
  #file based on the use of Hydrologic Response Units (HRUs).
  
  #USAGE
  # make_veg_param_file(hru_df)
  
  #ARGUMENTS:
  #hru_table   - HRU attribute table as data frame
  
  #DETAILS:
  # ...
  
  #VALUE:
  # ...
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  require("plyr")
  
  #hru_df <- read.csv(hru_table)
  cells <- unique(hru_df$CELL_ID)
  
  tmp <- NULL
  out <- NULL
  
  #Loop through individual cells
  for (cell in cells){
    
    #sub-set by CELL_ID field and determine number of records by cell
    recs <- hru_df[which(hru_df$CELL_ID==cell),]
    no_recs <- length(recs[[1]])

    #Determine area fraction correction factor; will equal one if area fractions sum to one
    area_corr <- 1 + (1 - sum(recs$AREA_FRAC))/sum(recs$AREA_FRAC)
    
    #Sort records so that BAND_ID and CLASS are in ascending order
    #records_sort <- cell_records[with(cell_records, order(BAND_ID, CLASS)),]
    recs_sort <- arrange(recs, BAND_ID, CLASS)
  
    #Get unique BAND_ID values
    bands <- unique(recs_sort$BAND_ID)
    
    #Loop through individual records
    for (r in 1:no_recs){
      af_adj <- recs_sort$AREA_FRAC[r]*area_corr
      band_index <- which(bands == recs_sort$BAND_ID[r])-1
      entry <- c(recs_sort$CLASS[r], af_adj, 0.1, 0.1, 1.0, 0.65, 1.0, 0.25, band_index)
      tmp <- rbind(tmp, entry)
    }
    
    z <- list(CELL_ID=cell, NO_HRU=no_recs, hru=tmp)
    out <- c(out,z)
    tmp <- NULL
    
  }
  
  return(out)
  
}