source("helper.R")

createBartList <- function(distList, randomize, loadList = FALSE,
                           listDir = NULL){

  if (isTRUE(loadList)){
    
    balloonList <- readRDS(listDir)
    
    if (isTRUE(randomize)){
      
      reInd <- sample(length(balloonList$PopVals))
      
      balloonList$PopVals <- balloonList$PopVals[reInd]
      
      balloonList$balloonColors <- balloonList$balloonColors[reInd]
      
    }
    
  } else {
    
    PopVals <- unlist(lapply(seq_len(distList$diffBalloons), getPopVals,
                      distributionList = distList))
    
    if (any(distList$balloonColor == "red" || distList$balloonColor == "green")) {
      warning("Your distList$balloonColor contained \"red\" or \"green\". These colors will already be used when the balloons are   saved or pop. Thus you might want to change your color values.")
    }
    
    balloonColors <- rep(distList$balloonColor, distList$nBalloons)
    
    
    if (isTRUE(randomize)){
      
      reInd <- sample(length(PopVals))
      
      PopVals <- PopVals[reInd]
      
      balloonColors <- balloonColors[reInd]
      
    }
    
    balloonList <- list("PopVals" = PopVals,
                        "balloonColors" = balloonColors,
                        "nBalloons" = sum(distList$nBalloons),
                        "max.pop" = distList$max.pop)
  }
  
  balloonList
  
}

