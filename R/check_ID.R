#' Check if a User is Allowed to Participate Given his Id
#'
#' @param id string or numeric. The user id to check.
#' @param cntrlVals list of reactive values.Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param location string. Where to get the reference database from. Must be one
#' of "local", "dropbox" or "vector". "local" will load a txt or rds file.
#' "dropbox" will load a csv file from the indicated dropbox account and "vector"
#' assumes that you gave a vector of ids as input to idsVec.
#' @param fileSep string. Passed to the \code{\link{read.table}} sep argument.
#'  Must match the separater used in the database file.
#' @param fileHeader logical. Passed to the read.table header argument.
#' @param dropDir string. The directory in the dropbox from which to read in the
#' database. Must only be specified if location is set to "dropbox".
#' @param fileName string. The database file-name, including path. Must be
#' specified if location is one of "dropbox" or "local".
#' @param droptoken string. The name of the rds file containing the access tokens
#'  of the dropbox account from which the data should be loaded. If file is
#'  outside the app directory full directory must be included.
#' @param idsVec string. Vector of ids that are no longer allowed to participate.
#'  Must only be specified if location is set to "vector".
#' @param notAllowedId string. The name (default is "not allowed") of the page
#'  that should be displayed in case the given id is occurring in the database.
#'
#' @importFrom utils read.table
#' @return If id is also contained in database cntrlVals$page will be changed to
#'  notAllowedId. Does not have to be reassigned since it's a reactive value.
#' @export
checkId <- function(id, cntrlVals, location = "local", fileSep = ",",
                    fileHeader = TRUE, dropDir = NULL, fileName = NULL,
                    droptoken = NULL, idsVec = NULL,
                    notAllowedId = "not allowed"){
  # function to check whether a participant id is already in a given set
  # set can either be given as a vector, from a local dataframe or from dropbox

  if (location == "local"){
    # determine function to read in data
    if (length(grep(".rds", fileName)) > 0){
      ids.df <- readRDS(file.path("www", fileName))
    } else {
      ids.df <- read.table(file.path("www", fileName), header = fileHeader,
                           sep = fileSep, stringsAsFactors = FALSE)
    }

  } else if (location == "dropbox"){

    # create path: note that this is the path in dropbox
    dtoken <- readRDS(droptoken)
    dropFilePath <- file.path(dropDir, fileName)
    ids.df <- rdrop2::drop_read_csv(dropFilePath, dtoken = dtoken,
                                    sep = fileSep, stringsAsFactors = FALSE) # read file from dropbox


  } else if (location == "vector"){
    idsVec <- as.character(idsVec)
    ids.df <- data.frame("ids" = idsVec)

  } else {
    stop(paste("No valid input file specified. Your entered location was",
               location, ". Choose between local, dropbox or vector."))
  }

  # check id
  if (gsub("[[:space:]]", "", tolower(as.character(id))) %in% tolower(ids.df[, 1])){
    cntrlVals$page <- notAllowedId
    cntrlVals$proceed <- 0
  }

}
