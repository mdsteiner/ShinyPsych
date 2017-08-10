dfeCustomMessage <- function(Opts, contrlVals, sess, containerOb, signCols){
  
  if (Opts == 2){
    
    sess$sendCustomMessage(type = 'envDfeTwoOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))
  } else if (Opts == 3){
    
    sess$sendCustomMessage(type = 'envDfeThreeOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                enThree = containerOb$outcomes[[contrlVals$dfeGamble]][[3]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))
    
  } else if (Opts == 4){
    
    sess$sendCustomMessage(type = 'envDfeFourOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                enThree = containerOb$outcomes[[contrlVals$dfeGamble]][[3]],
                                enFour = containerOb$outcomes[[contrlVals$dfeGamble]][[4]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))
    
  } else if (Opts == 5){
    
    sess$sendCustomMessage(type = 'envDfeFiveOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                enThree = containerOb$outcomes[[contrlVals$dfeGamble]][[3]],
                                enFour = containerOb$outcomes[[contrlVals$dfeGamble]][[4]],
                                enFive = containerOb$outcomes[[contrlVals$dfeGamble]][[5]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))
    
  } else if (Opts == 6){
    
    sess$sendCustomMessage(type = 'envDfeSixOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                enThree = containerOb$outcomes[[contrlVals$dfeGamble]][[3]],
                                enFour = containerOb$outcomes[[contrlVals$dfeGamble]][[4]],
                                enFive = containerOb$outcomes[[contrlVals$dfeGamble]][[5]],
                                enSix = containerOb$outcomes[[contrlVals$dfeGamble]][[6]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))
    
  } else {
    
    stop(paste(Opts, "is no valid input for nOpts. Must be a number between 2 and 6."))
    
  }
}