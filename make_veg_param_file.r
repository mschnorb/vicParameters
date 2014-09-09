make_veg_param_file <- function(hru_table){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #DESCRIPTION
  # Create vegetation parameter file for VIC model version 4.1.2glacier; vegetation parameter
  #file based on the use of Hydrologic Response Units (HRUs).
  
  #USAGE
  # make_veg_param_file(hru_table)
  
  #ARGUMENTS:
  #hru_table   - string giving full path to HRU attribute table
  
  #DETAILS:
  # ...
  
  #VALUE:
  # 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  require("plyr")
  
  hru_df <- read.csv(hru_table)
  cells <- unique(hru_df$CELL_ID)
  
  for (cell in cells){
    cell_records <- hru_df[which(hru_df$CELL_ID==cell),]
    #cell_records_sort <- cell_records[with(cell_records, order(BAND_ID, CLASS)),]
    cell_records_sort <- arrange(cell_records, BAND_ID, CLASS)
    no_records <- length(cell_records_sort[[1]])
  }
  
  
}