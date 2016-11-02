summarizeHRU <- function(hru_df,
                         qc = FALSE, 
                         minBand = 50){
  
  #DESCRIPTION: Check hru data frame for errors/discrepancies and fix; provide some hru statistics
  
  #ARGUMENT(S):
  # hru_df -  data frame of HRUs
  # qc -      quality control flag; default is FALSE 
  # minBand - minimum band ID
  
  require("plyr")
  
  if (qc) {
    #Check for elelvation errors
    hru_df$BAND_ID[which(hru_df$BAND_ID < minBand)] <- minBand
    hru_df$ELEVATION[which(hru_df$ELEVATION < 0)] <- 0
  }
  
  #Calculate and print statistics
  print(paste("  Minimum elevation:", min(hru_df$ELEVATION), sep=" "))
  print(paste("  Maximum elevation:", max(hru_df$ELEVATION), sep=" "))
  print(paste("    Minimum band ID:", min(hru_df$BAND_ID), sep=" "))
  print(paste("    Maximum band ID:", max(hru_df$BAND_ID), sep=" "))
  print(paste(" Maximum band range:", max(ddply(hru_df, .(CELL_ID), summarize, range=max(BAND_ID)-min(BAND_ID))$range), sep=" "))
  print(paste("         Total area:", sum(hru_df$AREA), sep=" "))
  print(paste("    Number of cells:", length(unique(hru_df$CELL_ID)), sep=" "))
  print(paste("  Average cell size:", sum(hru_df$AREA)/length(unique(hru_df$CELL_ID)), sep=" "))
  print(paste("  Number of classes:", length(unique(hru_df$CLASS)), sep=" "))
  print(paste("     Number of HRUs:", length(hru_df$CELL_ID), sep=" "))
  
  if (qc) return(hru_df) else return(NULL)
  
}