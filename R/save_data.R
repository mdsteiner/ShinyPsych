#' Write Data to a Local Location or to Dropbox
#'
#' Take a list of a specified format and save it to the specified location. All
#' elements in the list must be either of same length or of length 1 because
#' \code{\link[base]{as.data.frame}} will be called on the list. The file name is
#' created by pasting partId, Sys.time(), digest(data) and the suffix (and of
#' course the file specification .rds or .csv).
#' @param data list. Contains all the data that should be saved.
#' @param location string. The location to which data should be saved. Valid
#'  inputs are "local" or "dropbox".
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
#' @importFrom utils write.csv
#'
#' @return incProcess for shiny to display a process bar. Data is written out.
#' @export
#'
#' @examples
saveData <- function(data, location, partId, checkNull = TRUE,
                     addNameList = NULL, suffix = "_s",
                     outputDir = NULL, droptoken = "droptoken.rds",
                     asrds = FALSE, separator = ","){
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
  	# Create a unique file name for data
  	DatafileName <- paste0(partId,
  	                       as.integer(Sys.time()),
  	                       digest::digest(data.df),
  	                       suffix,
  	                       ".csv")

  	shiny::incProgress(.5)

  	if(location == "dropbox") {

  	  DatafilePath <- file.path(tempdir(), DatafileName)
  	  write.csv(data.df, DatafilePath, row.names = FALSE, quote = TRUE)

  	  dtoken <- readRDS(droptoken)

  	  # Upload data to dropbox using
  	  rdrop2::drop_upload(DatafilePath,
  	                      dest = outputDir,     # File directory in dropbox
  	                      dtoken = dtoken)   # Unique dropbox token

  	} else if (location == "local"){

  	  if (!is.null(outputDir)){
  	    DatafilePath <- file.path(outputDir, DatafileName)
  	  } else {
  	    DatafilePath <- file.path(DatafileName)
  	  }

  	  if (asrds == TRUE) {
  	    saveRDS(data.df, DatafilePath)
  	  } else {
  	    write.csv(data.df, DatafilePath, row.names = FALSE, quote = TRUE,
  	              sep = separator)
  	  }

  	} else {
  	  stop("No valid location entered. You entered", location, "as location. Location needs to be \"dropbox\" or \"local\".")
  	}

}