#' Call Appropriate Create Dfe Page Function and Send Data to Browser
#'
#' Depending on the number of options the gamble has, different create page
#' functions are needed and different information has to be sent to the browser.
#' This function takes care of this such that independend of the number of options
#' only one function has to be called to set up the html and javascript logic.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param session The session object of the shiny app.
#' @param container list. Output of \code{\link{createDfeList}} containing the
#'  outcome values to be displayed in a trial.
#' @param signalColors numeric. Controls the color of the outcome values
#'  displayed. There are two options: 1 means negative values will be displayed
#'  in red zeros in gray and positive values in green. A value other than 1 means
#'  that all outcomes are displayed in black.
#' @param ... Further arguments passed to the nOptDfePage function called (e.g.
#'  \code{\link{twoOptDfePage}}).
#'
#' @return An html page that can be displayed by the shiny app. Also information
#'  will be sent to javascript to animate the page.
#' @export
multiOptsDfePage <- function(ctrlVals, session, container, signalColors, ...) {

  if (container$nOpts >= 2 && container$nOpts <= 6){

    .dfeCustomMessage(Opts = container$nOpts, contrlVals = ctrlVals,
                     sess = session, containerOb = container,
                     signCols = signalColors)

    dfeFunNames <- c("twoOptDfePage", "threeOptDfePage", "fourOptDfePage",
                     "fiveOptDfePage", "sixOptDfePage")

    dfePage <- get(dfeFunNames[container$nOpts-1])(ctrlVals, ...)

  } else {

    stop(paste(container$nOpts,
               "is no valid input for nOpts. It must be a number between 2 and 6."))
  }

  dfePage

}
