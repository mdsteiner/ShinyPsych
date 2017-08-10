onBartAction <- function(id, ctrlVals, input, bartCtrlList,
                         balloonList, nextPageId = NULL){
  
  if (id == "nextballoon"){
    
    ctrlVals$pumps <- 0
    ctrlVals$pop <- 0
    ctrlVals$balloon <- ctrlVals$balloon + ctrlVals$proceed
    ctrlVals$proceed <- 0
    ctrlVals$saveballoon <- 0
    
  } else if (id == "next_page"){
    
    if(ctrlVals$balloon > balloonList$nBalloons) {
      
      ctrlVals$page <- nextPageId
      
    }
    
  } else if (id == "popped"){
    
    if (input$popped == 1) {
      
      ctrlVals$pop <- 1
      ctrlVals$pumps <- input$pumps
      
      bartCtrlList$balloon <- c(bartCtrlList$balloon, rep(ctrlVals$balloon,
                                                        ctrlVals$pumps))
      bartCtrlList$time <- c(bartCtrlList$time, input$actionTimes)
      bartCtrlList$pumps <- c(bartCtrlList$pumps, 1:ctrlVals$pumps)
      bartCtrlList$action <- c(bartCtrlList$action, rep(1, ctrlVals$pumps))
      bartCtrlList$pop <- c(bartCtrlList$pop, rep(0, ctrlVals$pumps-1), 1)
      
    }
    
  } else if (id == "saveballoon"){
    
    ctrlVals$pumps <- input$pumps
    
    bartCtrlList$balloon <- c(bartCtrlList$balloon, rep(ctrlVals$balloon,
                                                      ctrlVals$pumps+1))
    bartCtrlList$time <- c(bartCtrlList$time, input$actionTimes)
    bartCtrlList$pumps <- c(bartCtrlList$pumps, 1:ctrlVals$pumps, NA)
    bartCtrlList$action <- c(bartCtrlList$action, rep(1, ctrlVals$pumps), 0)
    bartCtrlList$pop <- c(bartCtrlList$pop, rep(0, ctrlVals$pumps+1))
    
    # Add points for current balloon to point total
    ctrlVals$points.cum <- ctrlVals$points.cum + ctrlVals$pumps
    ctrlVals$saveballoon <- 1
    
  } else {
    
    stop(paste(id, "is no valid input for id. Must be one of \"nextballoon\", \"next_page\", \"popped\" or \"saveballoon\""))
    
  }
    
  
}