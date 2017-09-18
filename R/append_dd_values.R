#' Append Input from Delay Discounting Task to Task Control List
#'
#' After a trial in the delay discouting task is ended the data is appended to
#' the task control list (gameData) which has to be a reactive value (see
#' \code{\link[shiny]{reactiveValues}} or
#' \url{https://shiny.rstudio.com/reference/shiny/latest/reactiveValues.html}).
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param input The input object from a shiny app.
#' @param gameData list of reactive values. Specifically for the bandit task.
#'  Can be created with \code{\link{createTaskCtrlList}}.
#' @param container list. Containing at least an object named "trial.order".
#'  Usually this list is created with \code{\link{createDdList}}.
#' @param withPracticeTrial logical. If TRUE it is assumed that a practice trial
#'  was included. The page after that trial will be afterPracticePage.
#' @param afterPracticePage string. The name of the page to be shown
#'  after the practice trial. Only needs to be specified if withPracticeTrial
#'  is set to TRUE.
#' @param afterTrialPage string. The name of the page to be shown
#'  after the trials up to the second to last trial.
#' @param afterLastTrialPage string. The name of the page to be shown
#'  after the last trial..
#'
#' @return The changed gameData list of reactive values and change current page
#' value (note: does not have to be assigned when function is called).
#' @export
appendDdValues <- function(ctrlVals, input, gameData, container,
                          withPracticeTrial = TRUE,
                          afterPracticePage = "postPractice",
                          afterTrialPage = "endGame",
                          afterLastTrialPage = "lastEndGame"){

  index <- ctrlVals$ddTrial

  # append data to gameData list
  gameData$time <- c(gameData$time, input$respTime[length(input$respTime)])

  gameData$selected <- c(gameData$selected, input$selected[length(input$selected)])

  gameData$trial.order <- c(gameData$trial.order,
                             container$trial.order[index])

  # go to next page and add 1 to the current trial
  if (isTRUE(withPracticeTrial)){

    if (ctrlVals$ddTrial == 1){

      ctrlVals$page <- afterPracticePage

      ctrlVals$ddTrial <- ctrlVals$ddTrial + 1

    } else if (any(c(2:(container$nTrials - 1)) == ctrlVals$ddTrial)){

      ctrlVals$page <- afterTrialPage

      ctrlVals$ddTrial <- ctrlVals$ddTrial + 1

    } else {

      ctrlVals$page <- afterLastTrialPage

      ctrlVals$ddTrial <- ctrlVals$ddTrial + 1
    }
  } else{

    if (any(seq_len(container$nTrials - 1) == ctrlVals$ddTrial)){

      ctrlVals$page <- afterTrialPage

      ctrlVals$ddTrial <- ctrlVals$ddTrial + 1
    } else {

      ctrlVals$page <- afterLastTrialPage

      ctrlVals$ddTrial <- ctrlVals$ddTrial + 1

    }
  }
}
