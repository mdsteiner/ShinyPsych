source("helper.R")
# uses retimes package for exgaussian

createBanditList <- function(nArms = 2, roundDigits = 0, loadList = FALSE,
                             distList = NULL, differentDists = FALSE,
                             fileName = NULL){
  
  if (loadList == "complete list"){
    return(load(fileName))
    
  } else if (loadList == "dist info"){
    
    if (length(grep(".rds", fileName)) > 0){
      
      distList <- readRDS(fileName)
      
    } else if (length(grep(".RData", fileName)) > 0){
      
      load(fileName)
      
    }
  } else if (!is.null(fileName)){
    stop(paste(loadList, "is no valid loadList input. Must be either \"complete list\", \"dist info\" or \"NULL\" and if it is not NULL fileName must be specified."))
  }
  
  
  if (isTRUE(differentDists)){
    
    outcomes <- lapply(seq_along(distList$nTrials), getDiffDistVals1,
                       distributionList = distList, Arms = nArms,
                       nDigits = roundDigits)
    
  } else {
    
    outcomes <- getSameDistVals(dList = distList, armNumber = nArms,
                                rDigits = roundDigits)
    
  }
    
    locations.r <- matrix(NA, ncol = nArms, nrow = length(distList$nTrials))
    # Randomize option locations
    for (ga in seq_along(distList$nTrials)){
      locations.r[ga, 1:nArms] <- sample(1:nArms)
      outcomes[[ga]] <- outcomes[[ga]][,locations.r[ga,]]
    }
    
    option.order <- vector("list", length(distList$nTrials))
    for (rows in 1:nrow(locations.r)){
      temp.order <- paste(locations.r[rows,], collapse = ";")
      option.order[[rows]] <- rep(temp.order, distList$nTrials[rows])
    }
    
    option.order <- unlist(option.order)
    
    container <- list("option.order" = option.order,
                      "random.locations" = locations.r,
                      "distList" = distList,
                      "outcomes" = outcomes)
    
    container
}
