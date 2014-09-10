make_veg_param_file <- function(hru_df){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #DESCRIPTION
  # Creates formatted output of vegetation parameters for VIC model version 4.1.2glacier;
  #vegetation parameters based on the use of Hydrologic Response Units (HRUs).
  
  #USAGE
  # make_veg_param_file(hru_df)
  
  #ARGUMENTS:
  #hru_table   - HRU attribute table as data frame; must contain following fields: CLASS,
  #                CELL_ID, BAND_ID, AREA_FRAC and ELEVATION
  
  #DETAILS:
  # ...
  
  #VALUE:
  # Function returns 4-item list of VIC cell metadata and vegetation parameters. List items are
  #   as follows:
  #   $NO_CELLS:  single number of VIC cells in computational domain
  #   $CELL_ID:   vector of cell IDs for every cell in computational domain
  #   $NO_HRU:    vector giving number of HRUs per VIC cell
  #   $PARAM:     List, where each item (one per cell) contains NO_HRUx9 size matrix of HRU
  #                 parameters, with one record per HRU
  #
  # Function side-effect is to write default vegetation parameter file "vpf_default.txt" to
  #current working directory. 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #Load dependencies
  require("plyr")
  
  cells <- unique(hru_df$CELL_ID)   #Vector of unique cell IDs
  no_cells <- length(cells)
  
  #Initialize variables
  out_list <- NULL     #Function return value as list output
  cell_vec <- NULL     #Track cell IDs as vector
  no_recs_vec <- NULL  #Track number of HRUs by cell as vector
  hru <- NULL          #Single HRU parameter record
  params <- NULL       #Matrix of HRU parameter records for a given cell
  
  #Vegetation rooting parameters
  ## TODO - set these parameters as a function of vegetation class; supply as function argument?
  thick1 = 0.1   #Thickness of top root zone [m]
  thick2 = 1.0   #Thickness of middle root zone [m]
  thick3 = 1.0   #Thickness of bottom root zone [m]
  rfrac1 = 0.1   #Fraction of roots in top root zone [m]
  rfrac2 = 0.65  #Fraction of roots in middle root zone [m]
  rfrac3 = 0.25  #Fraction of roots in bottom root zone [m]
  
  #Loop through individual cells
  for (cell in cells){
    
    #sub-set input data frame by CELL_ID and determine number of records (i.e. HRUs) by cell
    recs <- hru_df[which(hru_df$CELL_ID==cell),]
    no_recs <- length(recs[[1]])

    #Ensure that area fractions sum to 1; correct for rounding errors if necessary
    #Determine area fraction correction factor (will equal one if area fractions sum to one)
    area_corr <- 1 + (1 - sum(recs$AREA_FRAC))/sum(recs$AREA_FRAC)
    
    #Sort records so that BAND_ID and CLASS are in ascending order
    recs_sort <- arrange(recs, BAND_ID, CLASS)
  
    #Get unique BAND_ID values (not yet used - required for building band file)
    bands <- unique(recs_sort$BAND_ID)
    
    #Loop through individual records
    for (r in 1:no_recs){
      af_adj <- recs_sort$AREA_FRAC[r]*area_corr
      
      #Determine band index - currently indexed from minimum BAND_ID per cell
      #TODO - should band index be based on common base elevation/BAND_ID?
      band_index <- which(bands == recs_sort$BAND_ID[r])-1
      
      hru <- rbind(hru, c(recs_sort$CLASS[r], af_adj, thick1, rfrac1, thick2, rfrac2, thick3, rfrac3, band_index))
    }
    
    cell_vec <- c(cell_vec, cell)
    no_recs_vec <- c(no_recs_vec, no_recs)
    params <- append(params, list(hru))
    hru <- NULL
    
  }
  
  out_list <- list(NO_CELLS=no_cells, CELL_ID=cell_vec, NO_HRU=no_recs_vec, PARAM=params)
  
  #Write HRU parameters to VIC-formatted file
  out_file <- file(description="vpf_default.txt", open="w")
  for (x in 1:no_cells){
    txt1 <- sprintf(c("%.0f","%5.0f"), c(out_list$CELL_ID[x], out_list$NO_HRU[x]))
    write(txt1, file=out_file, ncolumns=2)
    for (y in 1:out_list$NO_HRU[x]){
      txt2 <- sprintf(c("%8.0f","%15.12f","%5.2f","%5.2f","%5.2f","%5.2f","%5.2f","%5.2f","%4.0f"),
                     out_list$PARAM[[x]][y,])
      write(txt2, file=out_file, ncolumns=9)
    }
  }
  close(out_file)
  
  return(out_list)
  
}