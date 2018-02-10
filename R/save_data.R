#' Write Data to a Local Location or to Dropbox
#'
#' Take a list of a specified format and save it to the specified location. All
#' elements in the list must be either of same length or of length 1 because
#' \code{\link[base]{as.data.frame}} will be called on the list. The file name is
#' created by pasting partId, Sys.time(), digest(data) and the suffix (and of
#' course the file specification .rds or .csv).
#' @param data list. Contains all the data that should be saved.
#' @param location string. The location to which data should be saved. Valid
#'  inputs are "local" , "mail" or "dropbox".
#' @param partId string. The participant id. This will be pasted to the beginning
#'  of the filename. If no id should be part of the filename leave this, in this
#'  case a sample of length 9 from 1:9 will be drawn and used as first part of
#'  the filename.
#' @param checkNull logical. If TRUE (default), each value in the list that is
#'  NULL will be replaced by NA.
#' @param addNameList string. Vector with length of data containing the variable
#'  names that should be given to the data. If names should not be change just
#'  leave this as NULL.
#' @param suffix string. The suffix given to the name of the saved file. You
#'  could e.g. use "_s" (default) for the survey data and "_g" for the game data
#'  to have an easy distinction in the filenames.
#' @param outputDir string. Directory WITHOUT the file name in which to save the
#'  data.
#' @param droptoken string. Name of the rds file with the dropbox acces tokens.
#'  If this file is not in the app directory the full directory must be given.
#'  Only needed if location is set to "dropbox".
#' @param asrds logical. If TRUE (default is FALSE), data is saved as an rds
#'  file.
#' @param separator string. Separator that should be used to write the file.
#'  Default is ",".
#' @param mailSender string. Mail sender address. Passed to
#'  \code{\link[sendmailR]{sendmail}} from the sendmailR package.
#' @param mailReceiver string. Mail receiver address. Passed to
#'  \code{\link[sendmailR]{sendmail}} from the sendmailR package.
#' @param mailSubject string. The mail subject. Passed to
#'  \code{\link[sendmailR]{sendmail}} from the sendmailR package.
#' @param mailBody string. The mail body. Passed to
#'  \code{\link[sendmailR]{sendmail}} from the sendmailR package.
#'
#' @importFrom utils write.csv write.table
#' @importFrom sendmailR mime_part sendmail
#' @importFrom rdrop2 drop_upload
#' @importFrom digest digest
#'
#' @return incProcess for shiny to display a process bar. Data is written out.
#' @export
saveData <- function(data, location, partId, checkNull = TRUE,
                     addNameList = NULL, suffix = "_s",
                     outputDir = NULL, droptoken = "droptoken.rds",
                     asrds = FALSE, separator = ",",
                     mailSender = NULL, mailReceiver = NULL,
                     mailSubject = "ShinyPsych Data",
                     mailBody = "Data attached..."){
  # The function takes a list of data and a Location to store it to
  # and creates a dataframe that will be stored.

    if (checkNull){
      # get rid of NULLs by replacing them with NAs
      data.new <- lapply(data, .convertNull)
    }

    # create data frame
    data.df <- as.data.frame(data.new)

    if (!is.null(addNameList)) {
      # if column names should be changed replace old ones
      names(data.df) <- addNameList
    }

    if (missing(partId)) {
      parId <- paste0(sample(c(1:9, letters), 9), collapse = "")
    }


    shiny::incProgress(.5)

    if(location == "dropbox") {
      # Create a unique file name for data
      DatafileName <- paste0(partId,
                             as.integer(Sys.time()),
                             digest::digest(data.df),
                             suffix,
                             ".csv")
      DatafilePath <- file.path(tempdir(), DatafileName)
      write.csv(data.df, DatafilePath, row.names = FALSE, quote = TRUE)

      dtoken <- readRDS(droptoken)

      # Upload data to dropbox using
      rdrop2::drop_upload(DatafilePath,
                          path = outputDir,  # File directory in dropbox
                          dtoken = dtoken)   # Unique dropbox token

    } else if (location == "local"){

      if (isTRUE(asrds)) {

        # Create a unique file name for data
        DatafileName <- paste0(partId,
                               as.integer(Sys.time()),
                               digest::digest(data.df),
                               suffix,
                               ".rds")

        if (!is.null(outputDir)){
          DatafilePath <- file.path(outputDir, DatafileName)
        } else {
          DatafilePath <- file.path(DatafileName)
        }
        saveRDS(data.df, DatafilePath)

      } else {

        if (any(c(",", ";") == separator)){
        # Create a unique file name for data
        DatafileName <- paste0(partId,
                               as.integer(Sys.time()),
                               digest::digest(data.df),
                               suffix,
                               ".csv")
        } else {
          # Create a unique file name for data
          DatafileName <- paste0(partId,
                                 as.integer(Sys.time()),
                                 digest::digest(data.df),
                                 suffix,
                                 ".txt")
        }

        if (!is.null(outputDir)){
          DatafilePath <- file.path(outputDir, DatafileName)
        } else {
          DatafilePath <- file.path(DatafileName)
        }

        write.table(data.df, DatafilePath, row.names = FALSE, quote = TRUE,
                  sep = separator)
      }

    } else if (location == "mail"){


      from <- mailSender
      to <- mailReceiver
      subject <- mailSubject

      DatafilePath <- file.path(tempdir(), DatafileName)
      write.csv(data.df, DatafilePath, row.names = FALSE, quote = TRUE)

      attachmentObject <- sendmailR::mime_part(x = DatafilePath,
                                               name = DatafileName)
      body <- list(mailBody,attachmentObject)

      sendmailR::sendmail(from, to, subject, body,
                          control=list(smtpServer="ASPMX.L.GOOGLE.COM"))
    } else {
      stop("No valid location entered. You entered", location, "as location. Location needs to be \"dropbox\", \"local\" or \"mail\".")
    }

}
