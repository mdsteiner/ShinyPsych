nextBanditPage <- function(ctrlValues, distList, gameData,
                           withPracticeGame = TRUE){
  
  if (isTRUE(withPracticeGame)){
    
    if (ctrlValues$banditGame == 1){
      
      ctrlValues$page <- "postPractice"
      
      ctrlValues$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1
      
    } else if (any(c(2:(length(distList$nTrials) - 1)) == ctrlValues$banditGame)){
      
      ctrlValues$page <- "endGame"
      
      ctrlValues$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1
    } else {
      
      ctrlValues$page <- "lastEndGame"
      
      ctrlValues$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1
    }
  } else{
    
    if (any(seq_len(length(distList$nTrials) - 1) == ctrlValues$banditGame)){
      
      ctrlValues$page <- "endGame"
      
      ctrlValues$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1
    } else {
      
      ctrlValues$page <- "lastEndGame"
      
      ctrlValues$banditGame <- unique(gameData$game)[length(unique(gameData$game))] + 1
      
    }
  }
    

}
  