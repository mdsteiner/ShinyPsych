source("helper.R")


preparePageList <- function(defaulttxt = TRUE, defaultName = "REI",
                            location = "local", txtPath = NULL,
                            randomize = FALSE, globId = defaultName,
                            droptoken = "droptoken.rds"){
  
  if (isTRUE(defaulttxt)){
    
    dir <- list.files("www", pattern = defaultName)
    dir <- file.path("www", dir)
    df <- read.table(dir, header = TRUE, sep = "\t",
                     stringsAsFactors = FALSE)
    
  } else if (location == "local"){
    
    dir <- txtPath
    df <- read.table(dir, header = TRUE, sep = "\t",
                     stringsAsFactors = FALSE)
    
  } else if (location == "dropbox"){
    dtoken <- readRDS(droptoken)
    df <- rdrop2::drop_read_csv(txtPath, dtoken = dtoken,
                                        sep = "\t", stringsAsFactors = FALSE)
    
  } else {
    stop(paste(location), "is no valid location. If defaulttxt is FALSE location must either be \"local\" or \"dropbox\".")
  }
  
  choicesList <- df$choices
  
  if (any(!is.na(df$choices))){
    choicesList <- strsplit(as.character(df$choices), split = ",", fixed = TRUE)
    
    choiceNames <- strsplit(as.character(df$choiceNames), split = ",",
                            fixed = TRUE)

    choiceNames <- lapply(choiceNames, revArgsGsub, pattern = "NA",
                          replacement = NA)
  
    choicesList[!is.na(choicesList)] <- 
      lapply(choicesList[!is.na(choicesList)], as.numeric)
    
    for (i in seq_along(df$id)){
      if (any(is.na(choiceNames[[i]]))){
        next()
      } else {
        names(choicesList[[i]]) <- choiceNames[[i]]
      }
    }
  
  }
  
  df$placeholder[is.na(df$placeholder)] <- ""
  
  df$inline[df$inline != 1] <- FALSE
  df$inline[df$inline == 1] <- TRUE
  

  if(any(!is.na(df$questionIndex))){
    qInd <- 1:max(df$questionIndex, na.rm = TRUE)
    nmax <- max(qInd)
  } else {
      qInd <- nmax <- NA
  }
  
  if (isTRUE(randomize)){
    df$page[df$randomize == 1] <- sample(df$page[df$randomize == 1])
  }
  
  textOrQuestionnaireList <- list(
    "questionIndex" = qInd,
    "text" = df$text,
    "reverse" = df$reverse,
    "choices" = choicesList,
    "description" = df$describtion,
    "page" = df$page,
    "type" = df$type,
    "min" = df$min,
    "max" = df$max,
    "placeholder" = df$placeholder,
    "id" = paste0(globId, "_", df$id),
    "globId" = as.character(globId),
    "disabled" = df$disabled,
    "width" = df$width,
    "height" = df$height,
    "inline" = df$inline,
    "checkType" = df$checkType
  )
  
  ind <- substr(textOrQuestionnaireList$id,
                start = nchar(textOrQuestionnaireList$id) - 1,
                stop = nchar(textOrQuestionnaireList$id)) != "NA"
  
  textOrQuestionnaireList$obIds <- textOrQuestionnaireList$id[ind]
  
  textOrQuestionnaireList
}