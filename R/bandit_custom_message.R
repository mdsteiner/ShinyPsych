#' Send Custom Message for Bandit Task from R to Javascript in Browser
#'
#' Send the information needed for the bandit task from R to javascript. What
#' exact information has to be sent depends on the number of arms specified.
#' More on custom messages on
#' \url{https://shiny.rstudio.com/reference/shiny/latest/session.html}
#' @param Arms integer. The number of arms of the bandit. Has to be between 2
#'  and 6.
#' @param contrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param distribList list. Must at least contain the number of trials "nTrials"
#'  (usually the one used to create the distributions with
#'  \code{\link{createBanditList}}).
#' @param sess The session object from the shiny app.
#' @param containerOb list. Contains the outcomes of all bandit games.
#'  Usually this list is created with \code{\link{createBanditList}}.
#' @param rDigits integer. Is passed to \code{\link[base]{round}}. Indicates
#'  the number of digits the total point value displayed should be rounded to.
#'
#' @return An html page then displayed by the shiny app.
.banditCustomMessage <- function(Arms, contrlVals, distribList, sess, containerOb,
                                rDigits){

  if (Arms == 2){

    sess$sendCustomMessage(type = 'envBanditTwoArms',
                            list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1], # outcome values
                                 enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2], # outcome values
                                 nTrials = distribList$nTrials[contrlVals$banditGame], # number of trials
                                 game = contrlVals$banditGame, # number of current game
                                 nDigits = rDigits)) # number of digits to round total points to
  } else if (Arms == 3){

    sess$sendCustomMessage(type = 'envBanditThreeArms',
                           list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                enThree = containerOb$outcomes[[contrlVals$banditGame]][,3],
                                nTrials = distribList$nTrials[contrlVals$banditGame],
                                game = contrlVals$banditGame,
                                nDigits = rDigits))

  } else if (Arms == 4){

    sess$sendCustomMessage(type = 'envBanditFourArms',
                              list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                   enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                   enThree = containerOb$outcomes[[contrlVals$banditGame]][,3],
                                   enFour = containerOb$outcomes[[contrlVals$banditGame]][,4],
                                   nTrials = distribList$nTrials[contrlVals$banditGame],
                                   game = contrlVals$banditGame,
                                   nDigits = rDigits))

  } else if (Arms == 5){

    sess$sendCustomMessage(type = 'envBanditFiveArms',
                              list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                   enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                   enThree = containerOb$outcomes[[contrlVals$banditGame]][,3],
                                   enFour = containerOb$outcomes[[contrlVals$banditGame]][,4],
                                   enFive = containerOb$outcomes[[contrlVals$banditGame]][,5],
                                   nTrials = distribList$nTrials[contrlVals$banditGame],
                                   game = contrlVals$banditGame,
                                   nDigits = rDigits))

  } else if (Arms == 6){

    sess$sendCustomMessage(type = 'envBanditSixArms',
                              list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1],
                                   enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2],
                                   enThree = containerOb$outcomes[[contrlVals$banditGame]][,3],
                                   enFour = containerOb$outcomes[[contrlVals$banditGame]][,4],
                                   enFive = containerOb$outcomes[[contrlVals$banditGame]][,5],
                                   enSix = containerOb$outcomes[[contrlVals$banditGame]][,6],
                                   nTrials = distribList$nTrials[contrlVals$banditGame],
                                   game = contrlVals$banditGame,
                                   nDigits = rDigits))

  } else {

    stop(paste(Arms, "is no valid input for nArms. Must be a number between 2 and 6."))

  }
}
