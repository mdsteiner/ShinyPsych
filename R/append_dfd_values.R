#' Append Input from Decisions from Description Task to Task Control List
#'
#' After a gamble in the decisions from describtion task is ended the data is
#' appended to the task control list (gameData) which has to be a reactive value
#' (see \code{\link[shiny]{reactiveValues}} or
#' \url{https://shiny.rstudio.com/reference/shiny/latest/reactiveValues.html}).
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param input The input object from a shiny app.
#' @param gameData list of reactive values. Specifically for the bandit task.
#'  Can be created with \code{\link{createTaskCtrlList}}.
#' @param container list. Containing at least two objects named "trial.order"
#' and "gamle.order". Usually this list is created with \code{\link{createDfdList}}.
#' @param withPracticeGamble logical. If TRUE it is assumed that a practice
#'  gamble was included. The page after that trial will be afterPracticePage.
#' @param afterPracticePage string. The name of the page to be shown
#'  after the practice trial. Only needs to be specified if withPracticeGamble
#'  is set to TRUE.
#' @param afterGamblePage string. The name of the page to be shown
#'  after the trials up to the second to last trial.
#' @param afterLastGamblePage string. The name of the page to be shown
#'  after the last trial.
#'
#' @return changed gameData list of reactive values and change current page
#' value (note: does not have to be assigned when function is called).
#' @export
appendDfdValues <- function(ctrlVals, input, gameData, container,
                            withPracticeGamble = TRUE,
                            afterPracticePage = "postPractice",
                            afterGamblePage = "endGame",
                            afterLastGamblePage = "lastEndGame"){

  index <- ctrlVals$dfdGamble

  # append values to task control reactive list
  gameData$time <- c(gameData$time, input$respTime[length(input$respTime)])

  gameData$selected <- c(gameData$selected, input$selected[length(input$selected)])

  gameData$option.order <- c(gameData$option.order,
                             container$option.order[index])

  gameData$gamble.order <- c(gameData$gamble.order,
                             container$gamble.order[index])

  # change page and gamble index variable
  if (isTRUE(withPracticeGamble)){

    if (ctrlVals$dfdGamble == 1){

      ctrlVals$page <- afterPracticePage

      ctrlVals$dfdGamble <- ctrlVals$dfdGamble + 1

    } else if (any(c(2:(nrow(container$gamble.df) - 1)) == ctrlVals$dfdGamble)){

      ctrlVals$page <- afterGamblePage

      ctrlVals$dfdGamble <- ctrlVals$dfdGamble + 1

    } else {

      ctrlVals$page <- afterLastGamblePage

      ctrlVals$dfdGamble <- ctrlVals$dfdGamble + 1
    }
  } else{

    if (any(seq_len(nrow(container$gamble.df) - 1) == ctrlVals$dfdGamble)){

      ctrlVals$page <- afterGamblePage

      ctrlVals$dfdGamble <- ctrlVals$dfdGamble + 1
    } else {

      ctrlVals$page <- afterLastGamblePage

      ctrlVals$dfdGamble <- ctrlVals$dfdGamble + 1

    }
  }
}
