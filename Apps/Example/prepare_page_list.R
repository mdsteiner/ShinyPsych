library("shiny")
source("helper.R")


preparePageList <- function(defaulttxt = TRUE, defaultName = "REI",
                            location = "local", txtPath = NULL,
                            randomize = FALSE, globId = defaultName){
  
  if (isTRUE(defaulttxt)){
    
    dir <- list.files("www", pattern = defaultName)
    dir <- file.path("www", dir)
    
  } else {
    
    dir <- txtPath
    
  }
  
  df <- read.table(dir, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
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
  
  textOrQuestionnaireList <- list(
    "questionIndex" = qInd,
    "text" = df$text,
    "reverse" = df$reverse,
    "randomize" = randomize,
    "choices" = choicesList,
    "n" = nmax,
    "description" = df$describtion,
    "page" = df$page,
    "nResponseOptions" = df$nResponseOptions,
    "type" = df$type,
    "min" = df$min,
    "max" = df$max,
    "placeholder" = df$placeholder,
    "id" = paste0(globId, "_", df$id),
    "globId" = as.character(globId),
    "disabled" = df$disabled,
    "width" = df$width,
    "height" = df$height,
    "inline" = df$inline
  )
  
  # randomize pages or questions
  # if(randomize == TRUE) {
  #   
  #   textOrQuestionnaireList.order <- sample(1:textOrQuestionnaireList$n,
  #                                           size = textOrQuestionnaireList$n)
  #   textOrQuestionnaireList$text <- rei.survey$text[textOrQuestionnaireList]
  #   
  # } else {textOrQuestionnaireList.order <- 1:textOrQuestionnaireList$n}
  
  textOrQuestionnaireList
}