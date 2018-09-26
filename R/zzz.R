.onAttach <- function(libname, pkgname) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples",
                                          package = "ShinyPsych"))

  validExamplesMsg <-
    paste0(
      "Valid appNames are:\n'",
      paste(validExamples, collapse = "', '"),
      "'")

  packageStartupMessage("ShinyPsych v0.2.2. Email: markus.d.steiner@gmail.com")
  packageStartupMessage("ShinyPsych_Guide() opens the package guide.\nNote that the guide currently only works if you've used 'build_vignettes = TRUE'\nwhen installing the package.")
  packageStartupMessage("For demo apps call the 'callApp(appName, action)' function with action = 'run'\nto run a demo and action = 'show' to show the app code.")
  packageStartupMessage(validExamplesMsg)
}
