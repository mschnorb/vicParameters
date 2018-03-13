merge.hru.records <- function(record1, record2){
  
  #Description:
  # Merge fields of record1 with fields of record2
  
  #Arguments:
  #record1 - single record data frame
  #record2 - single record data frame
  
  #Details:
  # Merge record is equal to record1 for all fields except AREA,
  # where AREA in the merged record is the sum of the AREA fields
  # from both records
  
  #Value:
  # Single record with merged values
  
  record_merge <- record1
  record_merge$AREA <- record1$AREA + record2$AREA
  return(record_merge)
}