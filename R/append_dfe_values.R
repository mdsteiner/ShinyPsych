#' Append Input from Decisions from Experience Task to Task Control List
#'
#' After a gamble in the decisions from experience task is ended the data is
#' sent from the browser to the current r session and is then with this function
#' appended to the task control list (gameData) which has to be a reactive value
#' (see \code{\link[shiny]{reactiveValues}} or
#' \url{https://shiny.rstudio.com/reference/shiny/latest/reactiveValues.html}).
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param input The input object from a shiny app.
#' @param gameData list of reactive values.Sspecifically for the bandit task.
#'  Can be created with \code{\link{createTaskCtrlList}}.
#' @param container list. Containing at least two objects named "trial.order"
#'  and "gamle.order". Usually this list is created with
#'  \code{\link{createDfeList}}.
#' @param withPracticeGamble logical. If TRUE it is assumed that a practice
#'  gamble was included. The page after that trial will be afterPracticePage.
#' @param afterPracticePage string. The name of the page to be shown
#'  after the practice trial. Only needs to be specified if withPracticeGamble
#'  is set to TRUE.
#' @param afterGamblePage string. The name of the page to be shown after the
#'  trials up to the second to last trial.
#' @param afterLastGamblePage string. The name of the page to be shown after
#'  the last trial.
#'
#' @return gameData list of reactive values and change current page
#' value (note: does not have to be assigned when function is called).
#' @export
appendDfeValues <- function(ctrlVals, input, gameData, container,
                            withPracticeGamble = TRUE,
                            afterPracticePage = "postPractice",
                            afterGamblePage = "endGame",
                            afterLastGamblePage = "lastEndGame"){

  index <- (length(input$trial) -
              (length(input$gambleNr[input$gambleNr == ctrlVals$dfeGamble]) - 1)) :
    length(input$trial)

  # append values to task control reactive list
  gameData$trial <- c(gameData$trial, input$trial[index])

  gameData$time <- c(gameData$time, input$respTime[index])

  gameData$selected <- c(gameData$selected,
                          rep(input$selected[length(input$selected)],
                              length(index)))

  gameData$finalOutcome <- c(gameData$finalOutcome,
                          rep(input$finalOutcome[length(input$finalOutcome)],
                              length(index)))

  gameData$samples <- c(gameData$samples, input$samples[index])

  gameData$outcome <- c(gameData$outcome, input$outcome[index])

  gameData$gamble <- c(gameData$gamble, input$gambleNr[index])

  orderIndex <- (((gameData$gamble[length(gameData$gamble)] - 1) *
                    container$nDraws) + 1) :
    (((gameData$gamble[length(gameData$gamble)] - 1) * container$nDraws) +
       length(index))

  gameData$option.order <- c(gameData$option.order,
                             container$option.order[orderIndex])

  gameData$gamble.order <- c(gameData$gamble.order,
                             rep(container$gamble.order[ctrlVals$dfeGamble],
                                 length(index)))


  # change page and gamble index variable
  if (isTRUE(withPracticeGamble)){

    if (ctrlVals$dfeGamble == 1){

      ctrlVals$page <- afterPracticePage

      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1

    } else if (any(c(2:(nrow(container$gamble.df) - 1)) == ctrlVals$dfeGamble)){

      ctrlVals$page <- afterGamblePage

      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1

    } else {

      ctrlVals$page <- afterLastGamblePage

      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1
    }
  } else{

    if (any(seq_len(nrow(container$gamble.df) - 1) == ctrlVals$dfeGamble)){

      ctrlVals$page <- afterGamblePage

      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1
    } else {

      ctrlVals$page <- afterLastGamblePage

      ctrlVals$dfeGamble <- unique(gameData$gamble)[length(unique(gameData$gamble))] + 1

    }
  }
}
