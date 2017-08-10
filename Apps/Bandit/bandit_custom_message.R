banditCustomMessage <- function(Arms, contrlVals, distribList, sess, containerOb,
                                rDigits){
  
  if (Arms == 2){
    
    sess$sendCustomMessage(type = 'envBanditTwoArms',
                            list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                 enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                 nTrials = distribList$nTrials[contrlVals$banditGame],
                                 game = contrlVals$banditGame,
                                 nDigits = rDigits))
  } else if (Arms == 3){
    
    sess$sendCustomMessage(type = 'envBanditThreeArms',
                           list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                enThree = containerOb$outcomes[[contrlVals$banditGame]][,3],
                                nTrials = distribList$nTrials[contrlVals$banditGame],
                                game = contrlVals$banditGame,
                                nDigits = rDigits))
    
  } else if (Arms == 4){
    
    sess$sendCustomMessage(type = 'envBanditFourArms',
                              list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                   enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                   enThree = containerOb$outcomes[[contrlVals$banditGame]][,3],
                                   enFour = containerOb$outcomes[[contrlVals$banditGame]][,4],
                                   nTrials = distribList$nTrials[contrlVals$banditGame],
                                   game = contrlVals$banditGame,
                                   nDigits = rDigits))
    
  } else if (Arms == 5){
    
    sess$sendCustomMessage(type = 'envBanditFiveArms',
                              list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                   enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                   enThree = containerOb$outcomes[[contrlVals$banditGame]][,3],
                                   enFour = containerOb$outcomes[[contrlVals$banditGame]][,4],
                                   enFive = containerOb$outcomes[[contrlVals$banditGame]][,5],
                                   nTrials = distribList$nTrials[contrlVals$banditGame],
                                   game = contrlVals$banditGame,
                                   nDigits = rDigits))
    
  } else if (Arms == 6){
    
    sess$sendCustomMessage(type = 'envBanditSixArms',
                              list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                   enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                   enThree = containerOb$outcomes[[contrlVals$banditGame]][,3],
                                   enFour = containerOb$outcomes[[contrlVals$banditGame]][,4],
                                   enFive = containerOb$outcomes[[contrlVals$banditGame]][,5],
                                   enSix = containerOb$outcomes[[contrlVals$banditGame]][,6],
                                   nTrials = distribList$nTrials[contrlVals$banditGame],
                                   game = contrlVals$banditGame,
                                   nDigits = rDigits))
    
  } else {
    
    stop(paste(Arms, "is no valid input for nArms. Must be a number between 2 and 6."))
    
  }
}