appendBanditValues <- function(ctrlVals, distList, input, gameData){

  if (length(input$gameNr[input$gameNr == ctrlVals$banditGame]) ==
        distList$nTrials[ctrlVals$banditGame]){

      index <- (length(input$trial) -
                  length(input$gameNr[input$gameNr == ctrlVals$banditGame])) :
                  length(input$trial)
                
      shinyjs::toggle("continueBandit")
      
      gameData$trial <- c(gameData$trial, input$trial[index])
      
      gameData$time <- c(gameData$time, input$respTime[index])
      
      gameData$selection <- c(gameData$selection, input$selection[index])
      
      gameData$outcome <- c(gameData$outcome, input$outcome[index])
      
      gameData$points.cum <- c(gameData$points.cum, input$outcomeCum[index])
      
      gameData$game <- c(gameData$game, input$gameNr[index])
  }
  
}