#' Create a List of Reactive Values for Task Control
#'
#' Create a list of reactive values ((see \code{\link[shiny]{reactiveValues}} or
#' \url{https://shiny.rstudio.com/reference/shiny/latest/reactiveValues.html}))
#' for task control. Additional variables can be specified.
#' @param task string. The task name. Has to be a single string since
#'  for different tasks some variables are the same. If you have several tasks
#'  in the same app just call this function several times. Valid inputs are
#'  "bandit", "bart", "dfe", "dfd" and "dd".
#' @param oVar string. Vector with the name(s) of additional
#'  variables that should be added to ctrlList.
#' @param oVarVals sting or numeric. Vector containing the values that should be
#'  appended to the oVars variables. Must be the same length as oVars.
#'
#' @return A list of reactive values used for task control.
#' @export
#'
#' @examples
#' GameData <- createTaskCtrlList(task = "dfd")
#' print(shiny::isolate(shiny::reactiveValuesToList(GameData)))
#' rm(GameData)
createTaskCtrlList <- function(task, oVar = NULL, oVarVals = NULL){

  if (task == "bandit"){

    taskList <- shiny::reactiveValues(trial = c(),
                                      time = c(),
                                      selection = c(),
                                      outcome = c(),
                                      points.cum = c(),
                                      game = c())

  } else if (task == "bart"){

    taskList <- shiny::reactiveValues(balloon = c(),
                                      time = c(),
                                      pumps = c(),
                                      action = c(),
                                      pop = c())

  } else if (task == "dfe"){

    taskList <- shiny::reactiveValues(trial = c(),
                                      time = c(),
                                      samples = c(),
                                      selected = c(),
                                      finalOutcome = c(),
                                      outcome = c(),
                                      gamble = c(),
                                      option.order = c(),
                                      gamble.order = c())
  } else if (task == "dfd"){

    taskList <- shiny::reactiveValues(time = c(),
                                      selected = c(),
                                      option.order = c(),
                                      gamble.order = c())
  } else if (task == "dd"){

    taskList <- shiny::reactiveValues(time = c(),
                                      selected = c(),
                                      trial.order = c())
  }


  if (!is.null(oVar)){
    for (oV in seq_along(oVar)){
      taskList[[oVar[oV]]] <- oVarVals[oV]
    }
  }

  taskList

}
