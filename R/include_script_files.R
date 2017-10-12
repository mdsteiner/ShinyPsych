#' Load Javascript and CSS Files
#'
#' The different tasks for which ShinyPsych provides a framework often work with
#' javascript and are styled up a bit with css. This function loads the
#' appropriate scripts.
#' @param fileList string. Vector with names to specify the tasks for which
#'  the files should be loaded. Valid inputs are "bandit", "dfd", "dfe", "dd",
#'  "bart" or any combination of these.
#' @param globalScript logical. If TRUE (default) the global style
#'  script (not just used for the tasks) is loaded. Script does not need to be
#'  named in fileList.
#' @param nArms integer. The number of arms the bandit should have. Only needed
#'  if "bandit" is in fileList.
#' @param nOpts integer. The number of options in a trial are presented. Only
#'  needed if fileList contains one of "dfe" or "dfd". If "dfe" the number must
#'  be between 2 and 6, if "dfd" the number must be between 2 and 4.
#'
#' @return Loaded css and javascript scripts.
#' @export
includeScriptFiles <- function(fileList = "", globalScript = TRUE, nArms = 0,
                               nOpts = 0){

  returnList <- list()
  numberList <- c("One", "Two", "Three", "Four", "Five", "Six")

  if (isTRUE(globalScript)) {
    filGlobalS <- system.file("css", "globalStyle.css",
                       package = "ShinyPsych")
    returnList[["globalScript"]] <- shiny::includeCSS(filGlobalS)}

  if (any(fileList == "bandit")){

    if (any(2:4 == nArms)){
      filBanditS <- system.file("css", "banditTwoToFourArms_Style.css",
                         package = "ShinyPsych")
      returnList[["banditStyle"]] <- shiny::includeCSS(filBanditS)

    } else if (any(5:6 == nArms)){
      filBanditS <- system.file("css", "banditFiveToSixArms_Style.css",
                         package = "ShinyPsych")
      returnList[["banditStyle"]] <- shiny::includeCSS(filBanditS)

    } else {
      stop(paste(nArms,
                 "is no valid input for nArms. Must be a number between 2 and 6."))
    }
    filBanditJs <- system.file("javascript", paste0("bandit", numberList[nArms],
                                                    "Arms_FunctionsComp.js"),
                               package = "ShinyPsych")
    returnList[["banditFunctions"]] <- shiny::includeScript(filBanditJs)

  }

  if (any(fileList == "bart")){
    filBartJs <- system.file("javascript", "bart_FunctionsComp.js",
                             package = "ShinyPsych")
    returnList[["bartFunctions"]] <- shiny::includeScript(filBartJs)
  }

  if (any(fileList == "dfe")){

    if (any(2:4 == nOpts)){
      filDfeS <- system.file("css", "dfeTwoToFourOpts_Style.css",
                         package = "ShinyPsych")
      returnList[["dfeStyle"]] <- shiny::includeCSS(filDfeS)

    } else if (any(5:6 == nOpts)){
      filDfeS <- system.file("css", "dfeFiveToSixOpts_Style.css",
                         package = "ShinyPsych")
      returnList[["dfeStyle"]] <- shiny::includeCSS(filDfeS)

    } else {
      stop(paste(nOpts,
                 "is no valid input for nOpts. Must be a number between 2 and 6."))
    }
    filDfeJs <- system.file("javascript", paste0("dfe", numberList[nOpts],
                                            "Opts_FunctionsComp.js"),
                       package = "ShinyPsych")
    returnList[["dfeFunctions"]] <- shiny::includeScript(filDfeJs)

  }

  if (any(fileList == "dfd")){

    if (any(2:3 == nOpts)){
      filDfdS <- system.file("css", "dfdTwoToThreeOpts_Style.css",
                             package = "ShinyPsych")
      returnList[["dfdStyle"]] <- shiny::includeCSS(filDfdS)

    } else if (nOpts == 4){
      filDfdS <- system.file("css", "dfdFourOpts_Style.css",
                             package = "ShinyPsych")
      returnList[["dfdStyle"]] <- shiny::includeCSS(filDfdS)

    } else {
      stop(paste(nOpts,
                 "is no valid input for nOpts. Must be a number between 2 and 4."))
    }
    filDfdJs <- system.file("javascript", "dfd_FunctionsComp.js",
                            package = "ShinyPsych")
    returnList[["dfdFunctions"]] <- shiny::includeScript(filDfdJs)

  }

  if (any(fileList == "dd")){
    filDdS <- system.file("css", "dd_Style.css",
                          package = "ShinyPsych")
    returnList[["ddStyle"]] <- shiny::includeCSS(filDdS)
    filDdJs <- system.file("javascript", "dd_FunctionsComp.js",
                           package = "ShinyPsych")
    returnList[["ddFunctions"]] <- shiny::includeScript(filDdJs)

  }
  returnList

}
