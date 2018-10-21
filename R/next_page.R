#' Go to Next Page
#'
#' If all questions on a page list are answered or the participant has read all the
#' text, this function will (if properly set up, this has to be handled in the
#' app) be called if the continue button is pressed. It is possible to have an
#' id and password check to make sure a participant is allowed to participate.
#' @param pageId string. Current page list id.
#' @param ctrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param nextPageId string. Id of the next page list that should be displayed after the
#' last page of the current list.
#' @param pageList list. The one returned by \code{\link{createPageList}} or one
#'  of the same structure.
#' @param globId string. The global id of the set of pages or questionnaire as
#'  specified in \code{\link{createCtrlList}}.
#' @param checkAllowed string. Default is "none". "id" and "password are alsow allowed to
#'  use \code{\link{checkId}} or \code{\link{checkPassword}} respectively
#'  to make sure a participant is allowed to participate.
#' @param checkAllowedPage integer. The page number of the current page list
#'  after which the id or password should be checked. Only needed if checkAllowed is
#'  not "none".
#' @param checkIdVar string or numeric. The user id or password to check.
#' @param checkLocation string. Where to get the reference database from. Must
#'  be one of "local", "dropbox" or "vector". "local" will load a txt or rds file.
#'  "dropbox" will load a csv file from the indicated dropbox account and "vector"
#'  assumes that you gave a vector of ids or hashes as input to \code{checkIdsVec}.
#'  Only needed if checkAllowed is TRUE.
#' @param checkSep string. Passed to the \code{\link[utils]{read.table}} sep argument.
#'  Must match the separator used in the database file. Only needed if
#'  checkAllowed is not "none".
#' @param checkHeader logical. Passed to the read.table header argument. Only
#'  needed if checkAllowed is not "none".
#' @param checkDropDir string. The directory in the dropbox from which to read
#'  in the database. Must only be specified if checkLocation is set to "dropbox".
#'  Only needed if checkAllowed is not "none".
#' @param checkFileName string. The database file-name, including path. Must be
#'  specified if checkLocation is one of "dropbox" or "local".Only needed if
#'  checkAllowed is not "none".
#' @param checkDroptoken string. The name of the rds file containing the access
#'  tokens of the dropbox account from which the data should be loaded. If file
#'  is outside the app directory full directory must be included. Only needed if
#'  checkAllowed is not "none".
#' @param checkIdsVec string. Vector of ids that are no longer allowed to
#'  participate or hashed passwords. Must only be specified if checkLocation is set to "vector".
#'  Only needed if checkAllowed is not "none".
#' @param checkNotAllowedId string. The name (default is "not allowed") of the
#'  page that should be displayed in case the given id is occurring in the
#'  database or a wrong password is given. Only needed if checkAllowed is not "none".
#' @param inputList The input object of the shiny app.
#'
#' @return Updated ctrlVals. Doesn't need to be assigned, since it is a reactive
#'  value.
#' @export
nextPage <- function(pageId, ctrlVals, nextPageId, pageList, globId,
                     checkAllowed = "none", checkAllowedPage = 1,
                     checkIdVar = "workerid", checkLocation = "local",
                     checkSep = ",", checkHeader = TRUE, checkDropDir = NULL,
                     checkFileName = NULL, checkDroptoken = NULL,
                     checkIdsVec = NULL, checkNotAllowedId = "not allowed",
                     inputList = NULL){

  if(ctrlVals$page == pageId) {

    # create index for page number
    tempIndex <- paste0(globId, ".num")

    if (checkAllowed != "none" && checkAllowedPage == ctrlVals[[tempIndex]]){
      if (is.null(inputList)){
        stop('inputList must be given if checkAllowed is not "none".')
      }
      if (checkAllowed == "id"){
        # Check for valid id
        checkId(id = inputList[[paste0(globId, "_", checkIdVar)]], cntrlVals = ctrlVals,
                location = checkLocation, fileSep = checkSep,
                fileHeader = checkHeader, dropDir = checkDropDir,
                fileName = checkFileName, droptoken = checkDroptoken,
                idsVec = checkIdsVec, notAllowedId = checkNotAllowedId)
      } else {
        # Check for valid password
        checkPassword(password = inputList[[paste0(globId, "_", checkIdVar)]],
                      cntrlVals = ctrlVals, location = checkLocation,
                      dropDir = checkDropDir, fileName = checkFileName,
                      droptoken = checkDroptoken, PwVec = checkIdsVec,
                      notAllowedId = checkNotAllowedId)
      }
    }

    # add 1 to the pagenumber
    ctrlVals[[tempIndex]] <- ctrlVals[[tempIndex]] + ctrlVals$proceed

    # set proceed value to 0 so that multiple clicking doesn't lead to
    # skip pages
    ctrlVals$proceed <- 0

    # if no more pages are in pageList, proceed to next pageList
    if(ctrlVals[[tempIndex]] > max(pageList$page, na.rm = TRUE)) {
      ctrlVals$page <- nextPageId
    }

  }

}
