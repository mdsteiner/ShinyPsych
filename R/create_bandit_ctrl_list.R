#' Create a List of Reactive Values for Bandit Task Control
#'
#' Create a list of reactive values (see \code{\link[shiny]{reactiveValues}} or
#' \url{https://shiny.rstudio.com/reference/shiny/latest/reactiveValues.html})
#' to later store the task values in. Additional variables can be specified.
#' @param oVar string. Vector with names of additional variables you want to
#'  add to the control list.
#' @param oVarVals string or numeric. Vector with values that should be appended
#'  to the list of reactive values on the positions specified in oVar.
#'
#' @return A list of reactive values.
#' @export
createBanditCtrlList <- function(oVar = NULL, oVarVals = NULL){

  GameData <- shiny::reactiveValues(trial = c(),
                                    time = c(),
                                    selection = c(),
                                    outcome = c(),
                                    points.cum = c(),
                                    game = c())

  if (!is.null(oVar)){
    for (oV in seq_along(oVar)){
      GameData[[oVar[oV]]] <- oVarVals[oV]
    }
  }

  GameData
}
