#' Run example app or open example app code
#'
#' Run the specified example app locally in the browser or open the code file
#' to see the whole app code.
#' @param appName string. The name of the example app. Must be one of "Bandit",
#'  "BART", "CheckId", "DD", "DfD", "DfE", "Survey" or "TagsInput".
#' @param action string. Valid inputs are "run" or "show". Whether the app
#'  should be run or the code shown. The default action is to run the app.
#' @importFrom shiny runApp
#'
#' @return Run a shiny app or show its code.
#' @export
#'
#' @examples
#' \dontrun{
#' # run the survey app example
#' callApp("Survey", "run")
#'
#' # show code of the survey app example
#' callApp("Survey", "show")
#' }
callApp <- function(appName, action="run"){

  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples",
                                          package = "ShinyPsych"))

  validExamplesMsg <-
    paste0(
      "Valid appNames are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(appName) || !nzchar(appName) ||
      !appName %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  if (action == "run"){

    # find and launch the app
    appDir <- system.file("shiny-examples", appName, package = "ShinyPsych")
    shiny::runApp(appDir, display.mode = "normal")

  } else if (action == "show"){

    appDir <- system.file("shiny-examples", appName, "app.R",
                          package = "ShinyPsych")

    file.edit(appDir)

  } else {
    stop(paste(action, "is no valid input for \"action\". Must be either \"run\" or \"show\"."))
  }
}
