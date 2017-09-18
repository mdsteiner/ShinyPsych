#' Controls What Happens After a Bart Trial
#'
#' Controls which variables are stored in case of different events such as a
#' saved balloon or a popped balloon.
#' @param id string. Controls what action should be executed (e.g. append values
#'  or go to next page). Must be one of "nextballoon", "next_page", "popped" or
#'  saveballoon.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param inputList The input object from the shiny app.
#' @param bartCtrlList list of reactive values. Used for task control. Create
#'  with \code{\link{createTaskCtrlList}}.
#' @param balloonList list. Output of \code{\link{createBartList}} containing the
#'  game parameters.
#' @param nextPageId string. Id of next page that should be displayed. Only
#'  needed if id is "next_page".
#'
#' @return Updated ctrlVals and bartCtrlList. Doesn't need to be assigned, since
#'  both are reactive values.
#' @export
onBartAction <- function(id, ctrlVals, inputList, bartCtrlList,
                         balloonList, nextPageId = NULL){

  if (id == "nextballoon"){

    ctrlVals$pumps <- 0
    ctrlVals$pop <- 0
    ctrlVals$balloon <- ctrlVals$balloon + ctrlVals$proceed
    ctrlVals$proceed <- 0
    ctrlVals$saveballoon <- 0

  } else if (id == "next_page"){

    if(ctrlVals$balloon > balloonList$nBalloons) {

      ctrlVals$page <- nextPageId

    }

  } else if (id == "popped"){

    if (inputList$popped == 1) {

      ctrlVals$pop <- 1
      ctrlVals$pumps <- inputList$pumps

      bartCtrlList$balloon <- c(bartCtrlList$balloon, rep(ctrlVals$balloon,
                                                        ctrlVals$pumps))
      bartCtrlList$time <- c(bartCtrlList$time, inputList$actionTimes)
      bartCtrlList$pumps <- c(bartCtrlList$pumps, 1:ctrlVals$pumps)
      bartCtrlList$action <- c(bartCtrlList$action, rep(1, ctrlVals$pumps))
      bartCtrlList$pop <- c(bartCtrlList$pop, rep(0, ctrlVals$pumps-1), 1)

    }

  } else if (id == "saveballoon"){

    ctrlVals$pumps <- inputList$pumps

    bartCtrlList$balloon <- c(bartCtrlList$balloon, rep(ctrlVals$balloon,
                                                      ctrlVals$pumps+1))
    bartCtrlList$time <- c(bartCtrlList$time, inputList$actionTimes)
    bartCtrlList$pumps <- c(bartCtrlList$pumps, 1:ctrlVals$pumps, NA)
    bartCtrlList$action <- c(bartCtrlList$action, rep(1, ctrlVals$pumps), 0)
    bartCtrlList$pop <- c(bartCtrlList$pop, rep(0, ctrlVals$pumps+1))

    # Add points for current balloon to point total
    ctrlVals$points.cum <- ctrlVals$points.cum + ctrlVals$pumps
    ctrlVals$saveballoon <- 1

  } else {

    stop(paste(id, "is no valid input for id. Must be one of \"nextballoon\", \"next_page\", \"popped\" or \"saveballoon\""))

  }


}
