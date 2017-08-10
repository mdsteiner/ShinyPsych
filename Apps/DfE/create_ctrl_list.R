

createCtrlList <- function(firstPage, globIds, complCode, complName,
                           oVars = NULL, oVarVals = NULL, task = NULL){
  
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
  
  if (!is.null(nameVec)){
    for (nam in seq_along(nameVec)){
    
      # append the page number indices to the reactive list
      ctrlList[[nameVec[nam]]] <- 1
    }
  }
  
  if (!is.null(oVars)){
    for (oV in seq_along(oVars)){
      
      # append other by the user specified variables
      ctrlList[[oVars[oV]]] <- oVarVals[oV]
    }
  }
  
  if (!is.null(task)){
    
    if (any(task == "bandit")){
      ctrlList[["banditGame"]] <- 1
    }
    
    if (any(task == "bart")){
      
      ctrlList[["balloon"]] <- 1
      ctrlList[["pumps"]] <- 0
      ctrlList[["pop"]] <- 0
      ctrlList[["saveballoon"]] <- 0
      ctrlList[["points.cum"]] <- 0
      
    }
    
    if (any(task == "dfe")){
      ctrlList[["dfeGamble"]] <- 1
    }
    
  }

  ctrlList
  
}


