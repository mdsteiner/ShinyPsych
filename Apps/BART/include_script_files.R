# uses shiny

includeScriptFiles <- function(fileList, globalScript = TRUE, nArms = 0,
                               nOpts = 0){
  
  returnList <- list()
  numberList <- c("One", "Two", "Three", "Four", "Five", "Six")
  
  if (isTRUE(globalScript)) {
    returnList[["globalScript"]] <- shiny::includeCSS("globalStyle.css")}
  
  if (any(fileList == "bandit")){
    
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
  
  if (any(fileList == "bart")){
    returnList[["bartFunctions"]] <- shiny::includeScript("bart_FunctionsComp.js")
  }
  
  if (any(fileList == "dfe")){
    
    if (nOpts >= 2 && nOpts <= 4){
      
      returnList[["dfeStyle"]] <- shiny::includeCSS("dfeTwoToFourOpts_Style.CSS")
      
    } else if (any(c(5, 6) == nOpts)){
      
      returnList[["dfeStyle"]] <- shiny::includeCSS("dfeFiveToSixOpts_Style.CSS")
      
    } else {
      stop(paste(nOpts,
                 "is no valid input for nOpts. Must be a number between 2 and 6."))
    }
    
    returnList[["dfeFunctions"]] <- shiny::includeScript(paste0("dfe",
                                                                   numberList[nOpts],
                                                                   "Opts_FunctionsComp.js"))
    
  }
  
  returnList
  
}