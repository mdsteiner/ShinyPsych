#' Check if a User has entered a valid password
#'
#' @param password string or numeric. The user id to check.
#' @param cntrlVals list of reactive values. Controls the flow through the
#'  Experiment. Can be created with \code{\link{createCtrlList}}.
#' @param location string. Where to get the reference password/s from. Must be one
#' of "local", "dropbox" or "vector". "local" will load a txt or rds file.
#' "dropbox" will load a csv file from the indicated dropbox account and "vector"
#' assumes that you gave a vector of hashed passwords as input to PwVec.
#' @param dropDir string. The directory in the dropbox from which to read in the
#' database. Must only be specified if location is set to "dropbox".
#' @param fileName string. The database file-name, including path. Must be
#' specified if location is one of "dropbox" or "local".
#' @param droptoken string. The name of the rds file containing the access tokens
#'  of the dropbox account from which the data should be loaded. If file is
#'  outside the app directory full directory must be included.
#' @param PwVec string. Vector of valid hashed passwords.
#'  Must only be specified if location is set to "vector".
#' @param notAllowedId string. The name (default is "wrong password") of the page
#'  that should be displayed in case the given password is invalid.
#'
#' @importFrom utils read.table
#' @importFrom openssl openssl
#'
#' @return If the given password is not contained in the database, cntrlVals$page will be changed to
#'  notAllowedId. Does not have to be reassigned since it's a reactive value.
#' @export
checkPassword <- function(password, cntrlVals, location = "local", dropDir = NULL,
                          fileName = NULL, droptoken = NULL, PwVec = NULL,
                          notAllowedId = "wrong password"){
  # function to check a password input against a hashed password which
  # can either be given as a vector, from a local file or from dropbox

  if (location == "local"){
    # determine function to read in data
    if (length(grep(".rds", fileName)) > 0){
      pw.df <- readRDS(file.path("www", fileName))
    } else {
      pw.df <- read.table(file.path("www", fileName), header = FALSE,
                          stringsAsFactors = FALSE)
    }

  } else if (location == "dropbox"){

    # create path: note that this is the path in dropbox
    dtoken <- readRDS(droptoken)
    dropFilePath <- file.path(dropDir, fileName)
    pw.df <- rdrop2::drop_read_csv(dropFilePath, dtoken = dtoken,
                                   stringsAsFactors = FALSE) # read file from dropbox


  } else if (location == "vector"){
    PwVec <- as.character(PwVec)
    pw.df <- data.frame("passwords" = PwVec)

  } else {
    stop(paste("No valid input file specified. Your entered location was",
               location, ". Choose between local, dropbox or vector."))
  }

  # check password
  if (!openssl::sha512(password) %in% pw.df[, 1]){
    cntrlVals$page <- notAllowedId
    cntrlVals$proceed <- 0
  }

}
