#' Create a List to Control the Flow through Experiment
#'
#' Create a list of reactive values (see \code{\link[shiny]{reactiveValues}} or
#' \url{https://shiny.rstudio.com/reference/shiny/latest/reactiveValues.html})
#' with important variables needed to go through an experiment. Additional
#' variables can be specified.
#' @param firstPage string. The id of the first page to be displayed
#' @param globIds string. A vector containing the ids of the
#'  different pages to be displayed (such as instructions or questionnaires but
#'  not tasks). Only pages that have a list either created by
#'  \code{\link{createPageList}} or are in the same form have to be indicated
#'  here. A hardcoded single page has not to be indicated here.
#' @param complCode logical. If TRUE (default is FALSE) a completion code
#'  will be created and complName should be specified.
#' @param complName string. Will be the beginning of the completion code.
#'  The completion code will be in the form "complName-XXX-XXX-XXX" where XXX is
#'  a randomly drawn number between 100 and 999. This code can then be displayed
#'  to the participant that he can enter e.g. as code on Amazon MTurk.
#' @param oVars string. Vector with the name(s) of additional
#'  variables that should be added to ctrlList.
#' @param oVarVals sting or numeric. Vector containing the values that should be
#'  appended to the oVars variables. Must be the same length as oVars.
#' @param task string. Vector with names indicating which task will be
#'  played in the app. Valid inputs are: "bandit", "bart" "dfe", "dfd" or "dd"
#'  or any combination of these. For each given task the respective
#'  variables necessary will be appended.
#'
#' @return A list of reactive values to control the experiment.
#' @export
#'
createCtrlList <- function(firstPage, globIds, complCode = FALSE, complName = NULL,
                           oVars = NULL, oVarVals = NULL, task = NULL){

  # create the names for later indexing the page number
  nameVec <- paste0(globIds, ".num")

  # create reactive value
  ctrlList <- shiny::reactiveValues(page = firstPage,
                                    proceed = 0)

  if (isTRUE(complCode)){

    ctrlList[["completion.code"]] <- paste(complName, sample(100:999, size = 1),
                                           sample(100:999, size = 1),
                                           sample(100:999, size = 1), sep = "-")
  }

  if (!is.null(nameVec)){
    for (nam in seq_along(nameVec)){

      # append the page number indices to the reactive list
      ctrlList[[nameVec[nam]]] <- 1
    }
  }

  if (!is.null(oVars)){
    for (oV in seq_along(oVars)){

      # append other by the user specified variables
      ctrlList[[oVars[oV]]] <- oVarVals[oV]
    }
  }

  if (!is.null(task)){

    if (any(task == "bandit")){
      ctrlList[["banditGame"]] <- 1
    }

    if (any(task == "bart")){

      ctrlList[["balloon"]] <- 1
      ctrlList[["pumps"]] <- 0
      ctrlList[["pop"]] <- 0
      ctrlList[["saveballoon"]] <- 0
      ctrlList[["points.cum"]] <- 0

    }

    if (any(task == "dfe")){
      ctrlList[["dfeGamble"]] <- 1
    }

    if (any(task == "dfd")){
      ctrlList[["dfdGamble"]] <- 1
    }

    if (any(task == "dd")){
      ctrlList[["ddTrial"]] <- 1
    }

  }

  ctrlList

}


