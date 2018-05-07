###########################################################################################################
summarizeHRU <- function(x){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION: Summarize an hru table by CLASS and by BAND_ID
  
  #ARGUMENT(S):
  # x - hru table as data frame
  
  #VALUE:
  # Function returns a list containing two data frames:
  #   d1 returns summary of HRUs by CLASS, including total area for each class, median elevation of class,
  #     number of HRUs in each class, and area fraction of each class
  #   d2 returns summary of HRUs by BAND_ID, including total area of each elevation band, overall mean
  #     elevation of given band, number of HRUs in each band, area fraction of each band and cumulative
  #     area fraction of each band
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  require("plyr")
  
  d1 <- ddply(x, .(CLASS), summarize, AREA=sum(AREA), ELEV=median(ELEVATION), NO_HRU=length(BAND_ID))
  d1$AREA_FRAC <- d1$AREA/sum(d1$AREA)
  d2 <- ddply(x, .(BAND_ID), summarize, AREA=sum(AREA), ELEV=median(ELEVATION), NO_HRU=length(CLASS))
  d2$AREA_FRAC <- d2$AREA/sum(d2$AREA)
  d2$CUM_AREA_FRAC <- cumsum(d2$AREA_FRAC)
  
  return(list(d1,d2))
  
}


###########################################################################################################
summarizeHRUbyCell <- function(x){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION: Summarize an hru table by CELL_ID
  
  #ARGUMENT(S):
  # x - hru table as data frame
  
  #VALUE:
  # Function returns a data frame containing the following fields:
  # CELL_ID -   Cell id number
  # AREA -      Total area of cell
  # ELEV -      mean elevation of cell (mean of band elevations)
  # No_CLASS -  Number of vegetation classes in each cell
  # NO_BAND -   Number of elevation bands in each cell
  # NO_HRU -    Number of HRUs in each cell
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  require("plyr")
  
  d1 <- ddply(x, .(CELL_ID), summarize,
              AREA=sum(AREA),
              ELEV=mean(ELEVATION),
              NO_CLASS = length(unique(CLASS)),
              NO_BAND=length(unique(BAND_ID)),
              NO_HRU=length(CLASS))
  
  return(d1)
  
}

###########################################################################################################
statsHRU <- function(hru_df,
                     qc = FALSE, 
                     minBand = 50){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION: Check HRU data frame for errors/discrepancies and fix; provide some hru statistics
  
  #ARGUMENT(S):
  # hru_df -  data frame of HRU table
  # qc -      quality control flag; default is FALSE 
  # minBand - minimum band ID
  
  #VALUE:
  # If qc = TRUE then return data frame with corrected ELEVATION and BAND_ID values, otherwise return NULL
  # Function always prints descriptive statistics to stdout
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  require("plyr")
  
  if (qc) {
    #Check for elelvation errors
    hru_df$BAND_ID[which(hru_df$BAND_ID < minBand)] <- minBand
    hru_df$ELEVATION[which(hru_df$ELEVATION < 0)] <- 0
  }
  
  #Calculate and print statistics
  print(paste("            Minimum elevation:", min(hru_df$ELEVATION), sep=" "))
  print(paste("            Maximum elevation:", max(hru_df$ELEVATION), sep=" "))
  print(paste("              Minimum band ID:", min(hru_df$BAND_ID), sep=" "))
  print(paste("              Maximum band ID:", max(hru_df$BAND_ID), sep=" "))
  print(paste("      Maximum cell band range:", max(ddply(hru_df, .(CELL_ID), summarize, range=max(BAND_ID)-min(BAND_ID))$range), sep=" "))
  print(paste("                   Total area:", sum(hru_df$AREA), sep=" "))
  print(paste("              Number of cells:", length(unique(hru_df$CELL_ID)), sep=" "))
  print(paste("            Average cell area:", sum(hru_df$AREA)/length(unique(hru_df$CELL_ID)), sep=" "))
  print(paste("            Number of classes:", length(unique(hru_df$CLASS)), sep=" "))
  print(paste("               Number of HRUs:", length(hru_df$CELL_ID), sep=" "))
  print(paste(" Mean number of HRUs per cell:", length(hru_df$CELL_ID)/length(unique(hru_df$CELL_ID)), sep=" "))
  print(paste("  Min number of HRUs per cell:", min(ddply(hru_df, .(CELL_ID), summarize, count=length(CLASS))$count), sep=" "))
  print(paste("  Max number of HRUs per cell:", max(ddply(hru_df, .(CELL_ID), summarize, count=length(CLASS))$count), sep=" "))
  
  if (qc) return(hru_df) else return(NULL)
  
}