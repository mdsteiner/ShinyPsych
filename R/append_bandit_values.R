#' Append Input from n Armed Bandit Task to Task Control List
#'
#' After a game in the bandit task is ended the data will be sent from the browser
#' to the current R session. This functions appends the sent data to task control
#' list (gameData) which has to be a reactive value (see
#' \code{\link[shiny]{reactiveValues}} or
#' \url{https://shiny.rstudio.com/reference/shiny/latest/reactiveValues.html}).
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param distList list. At least contains the number of trials "nTrials"
#'  (usually the one used to create the distributions with
#'  \code{\link{createBanditList}}).
#' @param input The input object from a shiny app.
#' @param gameData list of reactive values. Specifically for the bandit task.
#'  Can be created with \code{\link{createTaskCtrlList}}.
#'
#' @export
#' @return The changed gameData list of reactive values (note: does not have
#'  to be assigned when function is called).

appendBanditValues <- function(ctrlVals, distList, input, gameData){

  # only execute if the returned values match the expected number
  if (length(input$gameNr[input$gameNr == ctrlVals$banditGame]) ==
        distList$nTrials[ctrlVals$banditGame]){

      index <- (length(input$trial) -
                  length(input$gameNr[input$gameNr == ctrlVals$banditGame])) :
                  length(input$trial)

      # enable the button to continue in the experiment
      shinyjs::toggle("continueBandit")

      gameData$trial <- c(gameData$trial, input$trial[index])

      gameData$time <- c(gameData$time, input$respTime[index])

      gameData$selection <- c(gameData$selection, input$selection[index])

      gameData$outcome <- c(gameData$outcome, input$outcome[index])

      gameData$points.cum <- c(gameData$points.cum, input$outcomeCum[index])

      gameData$game <- c(gameData$game, input$gameNr[index])
  }

}
