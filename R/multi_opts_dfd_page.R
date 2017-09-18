#' Call Appropriate Create Dfd Page Function
#'
#' Depending on the number of options a trial has, different create page
#' functions are needed. This function takes care of this such that independend
#' of the number of options only one function has to be called to set up the html
#' logic.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param container list. Output of \code{\link{createDfdList}} containing the
#'  gambles to be displayed in a trial.
#' @param ... Further arguments passed to the nOptDfdPage function called (e.g.
#'  \code{\link{twoOptDfdPage}}).
#'
#' @return An html page that can be displayed by the shiny app.
#' @export
multiOptsDfdPage <- function(ctrlVals, container, ...) {

  if (container$nOpts >= 2 && container$nOpts <= 4){

    dfdFunNames <- c("twoOptDfdPage", "threeOptDfdPage", "fourOptDfdPage")

    dfdPage <- get(dfdFunNames[container$nOpts-1])(ctrlVals, container, ...)

  } else {

    stop(paste(container$nOpts,
               "is no valid input for nOpts. It must be a number between 2 and 4."))
  }

  dfdPage

}
