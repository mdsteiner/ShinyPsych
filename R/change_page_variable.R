#' Change a Variable from a Page List
#'
#' Changes a variable from a page list. This can be used for example to include
#' a code that was generated in the current r session, or to display the prior input of a user.
#' @param pageList list. Contains information to build a shiny page. Usually
#'  created with \code{\link{createPageList}}.
#' @param variable string. The name of the variable to be changed.
#' @param oldLabel string. The value to be changed. Must match a current value of
#' the \code{variable} in the \code{pageList}.
#' @param newLabel An object that will replace the \code{oldLabel}.
#'
#' @return The given pageList with the updated value.
#' @export
changePageVariable <- function(pageList, variable, oldLabel, newLabel){

  pageList[[variable]][pageList[[variable]] == oldLabel] <- newLabel

  pageList

}
