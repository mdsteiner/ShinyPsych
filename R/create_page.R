#' Create HTML Logic for a Page Prepared with \code{\link{createPageList}}
#'
#' Create a html page from the list given to pageList.
#' @param pageList list. Either returned by \code{\link{createPageList}} or of
#'  the same structure.
#' @param pageNumber integer. The current page number of this page id.
#'  Usually the one returned by \code{\link{createCtrlList}} where
#'  the page id was specified in the globIds argument.
#' @param globId string. The global Id of the current page or questionnaire.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param continueButton logical. If TRUE (default) a continue button is added
#'  at the bottom of the page.
#'
#' @return An html page that can be displayed by the shiny app.
#' @export
createPage <- function(pageList, pageNumber, globId, ctrlVals,
                       continueButton = TRUE){
  # create pagelayout with all elements from a given page number of pageList

  index <- which(.subset2(pageList, "page") %in% c(0, pageNumber))

  thisPage <- lapply(index, .callTag, pageList = pageList)

  if (isTRUE(continueButton)){
    if (any(.subset2(pageList, "disabled")[index] == 1)){
      thisPage <- list(shiny::br(), shiny::br(), shiny::br(), thisPage,
                       shiny::br(), shinyjs::disabled(
                         shiny::actionButton(inputId = paste0(globId, "_next"),
                                             label = "Continue")))
    } else {
      thisPage <- list(shiny::br(), shiny::br(), shiny::br(), thisPage,
                       shiny::br(), shiny::actionButton(
                         inputId = paste0(globId, "_next"), label = "Continue"))
    }
  } else {
    thisPage <- list(shiny::br(), shiny::br(), shiny::br(), thisPage)
  }

  ctrlVals$proceed <- 1

  thisPage

}
