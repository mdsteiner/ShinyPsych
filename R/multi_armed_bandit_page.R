#' Call Appropriate Create Bandit Page Function and Send Data to Browser
#'
#' Depending on the number of armes the bandit has, different create page
#' functions are needed and different information has to be sent to the browser.
#' This function takes care of this such that independend of the number of arms
#' only one function has to be called to set up the html and javascript logic.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param nArms integer. Number of arms the bandit has. Must be between 2 and 6
#' @param distList list. The one used to create the outcome lists with
#'  \code{\link{createBanditList}}.
#' @param container list. Output of \code{\link{createBanditList}} containing the
#'  outcome values to be displayed in the game.
#' @param roundDigits integer. The number of digits after the comma the total
#'  point value should be rounded to.
#' @param session The session object from shiny
#' @param ... Further arguments passed to the nArmedBanditPage functions
#'  (e.g. \code{\link{twoArmedBanditPage}}).
#'
#' @return An html page that can be displayed by the shiny app. Also information
#'  is sent to javascript to animate the page.
#' @export
multiArmedBanditPage <- function(ctrlVals, nArms, distList, container,
                                 roundDigits, session, ...) {

  if (nArms >= 2 && nArms <= 6){

    .banditCustomMessage(Arms = nArms, contrlVals = ctrlVals,
                       distribList = distList, sess = session,
                       containerOb = container, rDigits = roundDigits)


    banditFunNames <- c("twoArmedBanditPage", "threeArmedBanditPage",
                        "fourArmedBanditPage", "fiveArmedBanditPage",
                        "sixArmedBanditPage")

    banditPage <- get(banditFunNames[nArms-1])(ctrlVals, ...)

  } else {

    stop(paste(nArms,
               "is no valid input for nArms. It must be a number between 2 and 6."))
  }

  banditPage

}
