getValues <- function(pageList, nmrc, inputList){
  
  if (isTRUE(nmrc)){
    tempVals <- unlist(lapply(pageList$obIds, function(x, inp) {
      as.numeric(inp[[x]])}
      , inp = inputList))
    
  } else {
    tempVals <- unlist(lapply(pageList$obIds, function(x, inp) {
      as.character(inp[[x]])}
      , inp = inputList))
  }
  
  if (any(pageList$reverse == 1)){
    
  tempRevVals <- pageList$reverse[pageList$id %in% pageList$obIds]
  
  maxVals <- unlist(unname(lapply(pageList$choices[pageList$id %in% pageList$obIds],
                                  max, na.rm = TRUE)))
  minVals <- unlist(unname(lapply(pageList$choices[pageList$id %in% pageList$obIds],
                                  min, na.rm = TRUE)))
  
  # Reverse coding
  tempVals[tempRevVals == 1] <- maxVals[tempRevVals == 1] +
    minVals[tempRevVals == 1] - tempVals[tempRevVals == 1]
  }
  
  tempVals
}