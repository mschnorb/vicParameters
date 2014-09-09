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
  
  hru_df <- read.csv(hru_table)
  
  
}