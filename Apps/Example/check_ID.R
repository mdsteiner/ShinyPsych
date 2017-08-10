

checkID <- function(id, cntrlVals, location = "local", sep = ",", header = TRUE, dropDir = NULL,
                    fileName = NULL, droptoken = NULL, ids.vec = NULL){
  # function to check whether a participant id is already in a given set
  # set can either be given as a vector, from a local dataframe or from dropbox
  
  if (location == "local"){
    # determine function to read in data
    if (length(grep(".rds", fileName)) > 0){
      ids.df <- readRDS(file.path("www", fileName))
    } else {
      ids.df <- read.table(file.path("www", fileName), header = header, sep = sep, 
                           stringsAsFactors = FALSE)
    }
    
  } else if (location == "dropbox"){
    
    # create path: note that this is the path in dropbox
    dropFilePath <- file.path(dropDir, fileName)         
    ids.df <- rdrop2::drop_read_csv(dropFilePath, dtoken = droptoken,
                                    sep = sep, stringsAsFactors = FALSE) # read file from dropbox
    
    
  } else if (location == "vector"){
    ids.vec <- as.character(ids.vec)
    ids.df <- data.frame("ids" = ids.vec)
    
  } else {
    stop(paste("No valid input file specified. Your entered location was", location,  ". Choose between local, dropbox or vector."))
  }
  
  # check id
  if (gsub("[[:space:]]", "", tolower(as.character(id))) %in% tolower(ids.df[, 1])){
    check <- "not allowed"
  } else {
    check <- "allowed"
  }
  
  check
  
}