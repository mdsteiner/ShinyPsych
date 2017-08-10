# uses shiny

includeScriptFiles <- function(fileList, globalScript = TRUE, nArms = 0){
  
  returnList <- list()
  
  if (isTRUE(globalScript)) {
    returnList[["globalScript"]] <- shiny::includeCSS("globalStyle.css")}
  
  if (any(fileList == "bandit")){
    
    numberList <- c("One", "Two", "Three", "Four", "Five", "Six")
    if (nArms >= 2 && nArms <= 4){
      
      returnList[["banditStyle"]] <- shiny::includeCSS("banditTwoToFourArms_Style.CSS")
      
    } else if (any(c(5, 6) == nArms)){
      
      returnList[["banditStyle"]] <- shiny::includeCSS("banditFiveToSixArms_Style.CSS")
      
    } else {
      stop(paste(nArms,
                 "is no valid input for nArms. Must be a number between 2 and 6."))
    }
    
    returnList[["banditFunctions"]] <- shiny::includeScript(paste0("bandit",
                                                                 numberList[nArms],
                                                                 "Arms_FunctionsComp.js"))
    
  }
  
  returnList
  
}