#' Enable a Continue Button if a Specified Criterium is Reached
#'
#' Check if a criterium, specified in the page list file given or if default
#' loaded by \code{\link{createPageList}}, is met and if so enable the continue
#' button. If several criteria are given they must ALL be met to enable the
#' button.
#' @param pageId string. The current page id.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param pageList list. The one returned by \code{\link{createPageList}} or one
#'  of the same structure.
#' @param globId string. The global id of the set of pages or questionnaire as
#'  specified in \code{\link{createCtrlList}}.
#' @param inputList The input object of the shiny app.
#' @param charNum integer. The minimum number of characters that must be given
#'  to \code{\link[shiny]{textInput}} to enable the button. Only needed if
#'  the page contains a text input.
#'
#' @return Updated state of the specified button.
#' @export
onInputEnable <- function(pageId, ctrlVals, pageList, globId,
                          inputList, charNum = NULL){

  if(ctrlVals$page == pageId){

    checkInput <- pageList$id[pageList$page ==
                                ctrlVals[[paste0(globId, ".num")]] &
                                pageList$id %in% pageList$obIds &
                                pageList$disabled == 1]
    checkTypeTemp <- pageList$checkType[pageList$page ==
                                          ctrlVals[[paste0(globId, ".num")]] &
                               pageList$id %in% pageList$obIds &
                                 pageList$disabled == 1]
    if (length(checkTypeTemp > 0) && any(!is.na(checkTypeTemp))) {
      if(mean(unlist(lapply(seq_along(checkInput), .checkInputFun,
                            inList = inputList, checkType = checkTypeTemp,
                            charNum = charNum, checkInput = checkInput)),
              na.rm = TRUE) == 1){

        shinyjs::enable(paste0(globId, "_next"))
      }
    }
  }
}
