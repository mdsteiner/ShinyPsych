

createCtrlList <- function(firstPage, globIds, complCode, complName ){
  
  # create the names for later indexing the page number
  nameVec <- paste0(globIds, ".num")
  
  # create reactive value
  ctrlList <- shiny::reactiveValues(page = firstPage,
                                    proceed = 0)
  
  if (isTRUE(complCode)){
    
    ctrlList[["completion.code"]] <- paste(complName, sample(100:999, size = 1), 
                                           sample(100:999, size = 1),
                                           sample(100:999, size = 1), sep = "-")
  }
  
  for (nam in seq_along(nameVec)){
    
    # append the page number indices to the reactive list
    ctrlList[[nameVec[nam]]] <- 1
  }
  
  ctrlList
  
}


