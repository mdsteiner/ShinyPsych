library("shiny")
source("helper.R")

# save values module



saveData <- function(data, location, partId, checkNull = TRUE, addNameList = NULL, suffix = "_s",
                     outputDir = NULL, droptoken = NULL, asrds = FALSE){
	# The function takes a list of data and a Location to store it to
	# and creates a dataframe that will be stored.
    
	  if (checkNull){
	    # get rid of NULLs by replacing them with NAs
	  	data.new <- lapply(data, convertNull)
	  }
  
    # create data frame
  	data.df <- as.data.frame(data.new)
  	
	  if (!is.null(addNameList)) {
	    # if column names should be changed replace old ones
		  names(data.df) <- addNames
	  }
  	
  	if (missing(partId)) {
  	  parId <- paste0(sample(c(0:9, letters), 9), collapse = "")
  	}
  	# Create a unique file name for data
  	DatafileName <- paste0(partId,
  	                       as.integer(Sys.time()),
  	                       digest::digest(data.df),
  	                       suffix,
  	                       ".csv")
  	
  	incProgress(.5)
  	
  	if(location == "dropbox") {
  	  
  	  DatafilePath <- file.path(tempdir(), DatafileName)
  	  write.csv(data.df, DatafilePath, row.names = FALSE, quote = TRUE)
  	  
  	  # Upload data to dropbox using 
  	  rdrop2::drop_upload(DatafilePath,   
  	                      dest = outputDir,     # File directory in dropbox
  	                      dtoken = droptoken)   # Unique dropbox token
  	  
  	} else if (location == "local"){
  	  
  	  if (!is.null(outputDir)){
  	    DatafilePath <- file.path(outputDir, DatafileName)
  	  } else {
  	    DatafilePath <- file.path(DatafileName)
  	  }
  	  
  	  if (asrds == TRUE) {
  	    saveRDS(data.df, DatafilePath)
  	  } else {
  	    write.csv(data.df, DatafilePath, row.names = FALSE, quote = TRUE)
  	  }
  	  
  	} else if (location == "email"){
  	  
  	  # maybe sendmailR or mailR packages but it's probably tricky especially with lot's of mails being sent so the mail account might block further mails (especially gmail)
  	  warning("email is currently not working as saveDataLocation. No data has been stored. Use dropbox or local.")
  	}

}