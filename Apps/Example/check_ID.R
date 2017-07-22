library(shiny)

checkID <- function(input, output, session, id, location = "local", sep = ",", header = TRUE, expContrDir = NULL,
                    drfilename = NULL, droptoken = NULL, ids.vec = NULL){
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
    
    # create path; note that drfilename has to be a .csv file
    expContrFilePath <- file.path(expContrDir, drfilename)         
    ids.df <- rdrop2::drop_read_csv(expContrFilePath, dtoken = droptoken) # read file from dropbox
    
    
  } else if (location == "vector"){
    ids.vec <- as.character(ids.vec)
    ids.df <- data.frame("ids" = ids.vec)
    
  } else {
    warning("No valid input file specified. Choose between local, dropbox or vector.")
  }
  
  # check id
  if (gsub("[[:space:]]", "", tolower(as.character(id))) %in% tolower(ids.df[, 1])){
    check <- "not allowed"
  } else {
    check <- "allowed"
  }
  
  check
}