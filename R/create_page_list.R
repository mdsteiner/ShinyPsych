#' Read in Information on What to Display on a Page and Prepare it for Later Use.
#'
#' Reads in a document with read.table and prepares the input to be in the form
#' needed to later call \code{\link{createPage}} on it to create and display a
#' html page containing text and or various input format offered by shiny.
#'
#' Input files must have a very specific form described in detail in the
#' vignettes (for details call ShinyPsych_Guide())
#' and can be read in from a local place or from dropbox. If dropbox is used
#' you must specify tokens to access it (see \code{\link[rdrop2]{drop_read_csv}})
#' in an rds file.
#' Available default files are "Demographics", "Goodbye",
#' "Goodbye_BanditFullStudy", "Instructions_Bandit",
#' "Instructions_BanditFullStudy", "Instructions_Bart", "Instructions_CheckId",
#' "Instructions_Dd", "Instructions_Dfd", "Instructions_Dfe",
#' "Instructions_Survey", "Instructions_TagsInput", "REI_Inventory",
#' "Survey_BanditFullStudy", "Survey_Example" and "TagsInput_Example".
#'
#' @param defaulttxt logical. If TRUE (default) a default option is used.
#' @param location string. The file location. Valid inputs are
#'  "local" or "dropbox". Not needed if defaulttxt is TRUE.
#' @param fileName string. The name of the file to read in. If
#'  defaulttxt is TRUE this has to be one of the options specified in details.
#'  Else it has to be the file name inclusive full path in either dropbox or the
#'  local system.
#' @param randomize logical. If TRUE (default) the page variable of
#'  the items that have a 1 in the randomize variable should be randomly ordered.
#' @param globId string. Vector containing the names of the page or questionnaire
#'  variables.
#' @param droptoken string. The name of the file in which the access tokens for
#'  dropbox are stored. If this file is not in the app directory the full path
#'  must be specified. Must be an rds file.
#' @importFrom utils read.table
#'
#' @return A list containing many different parameters necessary to later create
#'  the page with \code{\link{createPage}}. See example for details.
#' @export
#'
#' @examples
#' example.survey <- createPageList(fileName = "Survey_Example", randomize = TRUE)
#' example.survey
#' rm(example.survey)
createPageList <- function(defaulttxt = TRUE, location = "local",
                           fileName = "Example", randomize = TRUE,
                           globId = fileName, droptoken = "droptoken.rds"){

  if (isTRUE(defaulttxt)){

    fil <- system.file("extdata", paste0(fileName, ".txt"),
                       package = "ShinyPsych")

    df <- read.table(fil, header = TRUE, sep = "\t",
                     stringsAsFactors = FALSE)

  } else if (location == "local"){

    dir <- fileName
    df <- read.table(dir, header = TRUE, sep = "\t",
                     stringsAsFactors = FALSE)

  } else if (location == "dropbox"){
    dtoken <- readRDS(droptoken)
    df <- rdrop2::drop_read_csv(fileName, dtoken = dtoken,
                                        sep = "\t", stringsAsFactors = FALSE)

  } else {
    stop(paste(location, "is no valid location. If defaulttxt is FALSE location must either be \"local\" or \"dropbox\"."))
  }

  choicesList <- df$choices

  if (any(!is.na(df$choices))){
    choicesList <- strsplit(as.character(df$choices), split = ",", fixed = TRUE)

    choiceNames <- strsplit(as.character(df$choiceNames), split = ",",
                            fixed = TRUE)

    choiceNames <- lapply(choiceNames, .revArgsGsub, pattern = "NA",
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

  if (isTRUE(randomize)){
    df$page[df$randomize == 1] <- sample(df$page[df$randomize == 1])

  }

  id.order <- df$id[order(df$page)]
  id.order <- paste(id.order[!is.na(id.order)], collapse = ";")

  textOrQuestionnaireList <- list(
    "text" = df$text,
    "reverse" = df$reverse,
    "choices" = choicesList,
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
    "checkType" = df$checkType,
    "defaultList" = defaulttxt,
    "id.order" = id.order
  )

  ind <- substr(textOrQuestionnaireList$id,
                start = nchar(textOrQuestionnaireList$id) - 1,
                stop = nchar(textOrQuestionnaireList$id)) != "NA"

  textOrQuestionnaireList$obIds <- textOrQuestionnaireList$id[ind]

  textOrQuestionnaireList
}
