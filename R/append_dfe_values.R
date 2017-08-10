appendDfeValues <- function(ctrlVals, input, gameData, container,
                            withPracticeGame = TRUE,
                            afterPracticePage = "postPractice",
                            afterGamblePage = "endGame",
                            afterLastGamblePage = "lastEndGame"){
  
    
  index <- (length(input$trial) -
              (length(input$gambleNr[input$gambleNr == ctrlVals$dfeGamble]) - 1)) :
    length(input$trial)
  
  gameData$trial <- c(gameData$trial, input$trial[index])
  
  gameData$time <- c(gameData$time, input$respTime[index])
  
  gameData$selected <- c(gameData$selected,
                          rep(input$selected[length(input$selected)],
                              length(index)))
  
  gameData$finalOutcome <- c(gameData$finalOutcome,
                          rep(input$finalOutcome[length(input$finalOutcome)],
                              length(index)))
  
  gameData$samples <- c(gameData$samples, input$samples[index])
  
  gameData$outcome <- c(gameData$outcome, input$outcome[index])
  
  gameData$gamble <- c(gameData$gamble, input$gambleNr[index])
  
  orderIndex <- (((gameData$gamble[length(gameData$gamble)] - 1) *
                    container$nDraws) + 1) :
    (((gameData$gamble[length(gameData$gamble)] - 1) * container$nDraws) +
       length(index))
  
  gameData$option.order <- c(gameData$option.order,
                             container$option.order[orderIndex])
  
  gameData$gamble.order <- c(gameData$gamble.order,
                             rep(container$gamble.order[ctrlVals$dfeGamble],
                                 length(index)))
    
  if (isTRUE(withPracticeGame)){
    
    if (ctrlVals$dfeGamble == 1){
      
      ctrlVals$page <- afterPracticePage
      
      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1
      
    } else if (any(c(2:(nrow(container$gamble.df) - 1)) == ctrlVals$dfeGamble)){
      
      ctrlVals$page <- afterGamblePage
      
      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1
      
    } else {
      
      ctrlVals$page <- afterLastGamblePage
      
      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1
    }
  } else{
    
    if (any(seq_len(nrow(container$gamble.df) - 1) == ctrlVals$dfeGamble)){
      
      ctrlVals$page <- afterGamblePage
      
      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1
    } else {
      
      ctrlVals$page <- afterLastGamblePage
      
      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1
      
    }
  }
}
