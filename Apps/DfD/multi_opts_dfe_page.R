# source the different bandit pages

multiOptsDfePage <- function(ctrlVals, session, container, signalColors, ...) {
  
  if (container$nOpts >= 2 && container$nOpts <= 6){
    
    dfeCustomMessage(Opts = container$nOpts, contrlVals = ctrlVals,
                     sess = session, containerOb = container,
                     signCols = signalColors)
    
    dfeFunNames <- c("twoOptDfePage", "threeOptDfePage", "fourOptDfePage",
                        "fiveOptDfePage", "sixOptDfePage")
    
    dfePage <- get(dfeFunNames[container$nOpts-1])(ctrlVals, ...)
    
  } else {
    
    stop(paste(container$nOpts, "is no valid input for nOpts. It must be a number between 2 and 6."))
  }
  
  dfePage
  
}