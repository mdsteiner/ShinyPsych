# source the different bandit pages
# source the bandit custom message page

multiArmedBanditPage <- function(ctrlVals, nArms, distList, container,
                                 roundDigits, session, ...) {
  
  if (nArms >= 2 && nArms <= 6){
    
    banditCustomMessage(Arms = nArms, contrlVals = ctrlVals,
                       distribList = distList, sess = session,
                       containerOb = container, rDigits = roundDigits)
    
    
    banditFunNames <- c("twoArmedBanditPage", "threeArmedBanditPage", "fourArmedBanditPage", "fiveArmedBanditPage",
                        "sixArmedBanditPage")
  
    banditPage <- get(banditFunNames[nArms-1])(ctrlVals, ...)
  
  } else {
    
    stop(paste(nArms, "is no valid input for nArms. It must be a number between 2 and 6."))
  }
  
  banditPage
  
}