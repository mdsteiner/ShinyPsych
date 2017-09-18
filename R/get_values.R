#' Get Questionnaire Answers from Shiny Input and Append Them to a List
#'
#' Takes answers from the shiny input objects and appends them to a list. For
#' this to work, pageList must be created with \code{\link{createPageList}} or
#' have the same structure. If there are reversed scales in a questionnaire it
#' will also reverse them.
#' @param pageList list. The page parameters, such as ids and text to
#'  display. Create this with \code{\link{createPageList}}.
#' @param nmrc logical. If TRUE it is assumed that input was saved as numbers
#'  and as.numeric will be called on the vector after processing. This
#'  ensures that it's possible to e.g. calculate a mean of the vector later on.
#'  If FALSE values will be returned as characters.
#' @param inputList The input object from the shiny app.
#'
#' @return A list containing the user inputs to the questionnaire.
#' @export
getValues <- function(pageList, nmrc, inputList){

  if (isTRUE(nmrc)){
    tempVals <- unlist(lapply(pageList$obIds, function(x, inp) {
      as.numeric(inp[[x]])}
      , inp = inputList))

  } else {
    tempVals <- unlist(lapply(pageList$obIds, function(x, inp) {
      as.character(inp[[x]])}
      , inp = inputList))
  }

  if (any(pageList$reverse == 1)){

  tempRevVals <- pageList$reverse[pageList$id %in% pageList$obIds]

  maxVals <- unlist(unname(lapply(pageList$choices[pageList$id %in% pageList$obIds],
                                  max, na.rm = TRUE)))
  minVals <- unlist(unname(lapply(pageList$choices[pageList$id %in% pageList$obIds],
                                  min, na.rm = TRUE)))

  # Reverse coding
  tempVals[tempRevVals == 1] <- maxVals[tempRevVals == 1] +
    minVals[tempRevVals == 1] - tempVals[tempRevVals == 1]
  }

  tempVals
}
