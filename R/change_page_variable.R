#' Change a Variable from a Page List that
#'
#' Changes a variable from a page list has to be changed e.g. because a code was
#' generated in the current r session.
#' @param pageList list. Contains information to build a shiny page. Usually
#'  created with \code{\link{createPageList}}.
#' @param variable string. The name of the variable to be changed.
#' @param oldLabel string. Must match the current value of the pageList that
#'  should be changed.
#' @param newLabel An object that will replace the oldLabel in the pageList.
#'
#' @return The given pageList with the updated value.
#' @export
changePageVariable <- function(pageList, variable, oldLabel, newLabel){

  pageList[[variable]][pageList[[variable]] == oldLabel] <- newLabel

  pageList

}
