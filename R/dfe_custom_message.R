#' Send Information from R Session to Browser
#'
#' Sends outcome values for the decisions from experience task, usually created
#' with \code{\link{createDfePage}}, to the browser where they are then
#' processed and displayed by using java script.
#' @param Opts integer. The number of options to choose from.
#' @param contrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param sess The session object from the shiny app.
#' @param containerOb list. Containing the outcomes. Can be created by calling
#'  \code{\link{createDfePage}}.
#' @param signCols numeric. Controls the color of the outcome values
#'  displayed. There are two options: 1 means negative values will be displayed
#'  in red zeros in gray and positive values in green. A value other than 1 means
#'  that all outcomes are displayed in black.
#'
#' @return Sends information to javascript where it is processed.
.dfeCustomMessage <- function(Opts, contrlVals, sess, containerOb, signCols){

  if (Opts == 2){

    sess$sendCustomMessage(type = 'envDfeTwoOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))
  } else if (Opts == 3){

    sess$sendCustomMessage(type = 'envDfeThreeOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                enThree = containerOb$outcomes[[contrlVals$dfeGamble]][[3]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))

  } else if (Opts == 4){

    sess$sendCustomMessage(type = 'envDfeFourOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                enThree = containerOb$outcomes[[contrlVals$dfeGamble]][[3]],
                                enFour = containerOb$outcomes[[contrlVals$dfeGamble]][[4]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))

  } else if (Opts == 5){

    sess$sendCustomMessage(type = 'envDfeFiveOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                enThree = containerOb$outcomes[[contrlVals$dfeGamble]][[3]],
                                enFour = containerOb$outcomes[[contrlVals$dfeGamble]][[4]],
                                enFive = containerOb$outcomes[[contrlVals$dfeGamble]][[5]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))

  } else if (Opts == 6){

    sess$sendCustomMessage(type = 'envDfeSixOpts',
                           list(enOne = containerOb$outcomes[[contrlVals$dfeGamble]][[1]],
                                enTwo = containerOb$outcomes[[contrlVals$dfeGamble]][[2]],
                                enThree = containerOb$outcomes[[contrlVals$dfeGamble]][[3]],
                                enFour = containerOb$outcomes[[contrlVals$dfeGamble]][[4]],
                                enFive = containerOb$outcomes[[contrlVals$dfeGamble]][[5]],
                                enSix = containerOb$outcomes[[contrlVals$dfeGamble]][[6]],
                                gambleNr = contrlVals$dfeGamble,
                                signalColors = signCols))

  } else {

    stop(paste(Opts, "is no valid input for nOpts. Must be a number between 2 and 6."))

  }
}
