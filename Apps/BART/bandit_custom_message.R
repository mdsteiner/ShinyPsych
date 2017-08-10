banditCustomMessage <- function(nArms, ctrlVals, distList, session, container,
                                roundDigits){
  
  if (nArms == 2){
    
  session$sendCustomMessage(type = 'envBanditTwoArms',
                            list(enOne = container$outcomes[[ctrlVals$banditGame]][,1],
                                 enThree = container$outcomes[[ctrlVals$banditGame]][,2],
                                 nTrials = distList$nTrials[ctrlVals$banditGame],
                                 game = ctrlVals$banditGame,
                                 nDigits = roundDigits))
  } else if (nArms == 3){
    
    session$sendCustomMessage(type = 'envBanditThreeArms',
                              list(enOne = container$outcomes[[ctrlVals$banditGame]][,1],
                                   enTwo = container$outcomes[[ctrlVals$banditGame]][,2],
                                   enThree = container$outcomes[[ctrlVals$banditGame]][,3],
                                   nTrials = distList$nTrials[ctrlVals$banditGame],
                                   game = ctrlVals$banditGame))
    
  } else if (nArms == 4){
    
    session$sendCustomMessage(type = 'envBanditFourArms',
                              list(enOne = container$outcomes[[ctrlVals$banditGame]][,1],
                                   enTwo = container$outcomes[[ctrlVals$banditGame]][,2],
                                   enThree = container$outcomes[[ctrlVals$banditGame]][,3],
                                   enFour = container$outcomes[[ctrlVals$banditGame]][,4],
                                   nTrials = distList$nTrials[ctrlVals$banditGame],
                                   game = ctrlVals$banditGame,
                                   nDigits = roundDigits))
    
  } else if (nArms == 5){
    
    session$sendCustomMessage(type = 'envBanditFiveArms',
                              list(enOne = container$outcomes[[ctrlVals$banditGame]][,1],
                                   enTwo = container$outcomes[[ctrlVals$banditGame]][,2],
                                   enThree = container$outcomes[[ctrlVals$banditGame]][,3],
                                   enFour = container$outcomes[[ctrlVals$banditGame]][,4],
                                   enFive = container$outcomes[[ctrlVals$banditGame]][,5],
                                   nTrials = distList$nTrials[ctrlVals$banditGame],
                                   game = ctrlVals$banditGame,
                                   nDigits = roundDigits))
    
  } else if (nArms == 6){
    
    session$sendCustomMessage(type = 'envBanditSixArms',
                              list(enOne = container$outcomes[[ctrlVals$banditGame]][,1],
                                   enTwo = container$outcomes[[ctrlVals$banditGame]][,2],
                                   enThree = container$outcomes[[ctrlVals$banditGame]][,3],
                                   enFour = container$outcomes[[ctrlVals$banditGame]][,4],
                                   enFive = container$outcomes[[ctrlVals$banditGame]][,5],
                                   enSix = container$outcomes[[ctrlVals$banditGame]][,6],
                                   nTrials = distList$nTrials[ctrlVals$banditGame],
                                   game = ctrlVals$banditGame,
                                   nDigits = roundDigits))
    
  } else {
    
    stop(paste(nArms, "is no valid input for nArms. Must be a number between 2 and 6."))
    
  }
}