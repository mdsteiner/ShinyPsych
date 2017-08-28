#' Create a List of Prepared Values to Display Later in a Delay Discounting Task
#'
#' Create a list of tables that are then later displayed. The values, i.e.
#' outcome and time etc. must be specified and given as input. For each outcome
#' time pair a still incomplete html table is created to later be completed and
#' displayed by \code{\link{ddPage}} (It's incomplete because some
#' parameters need to be specified later on).
#' @param loadFile logical. If TRUE (default) the list specifying
#'  the outcome time pairs will be loaded from a local file.
#' @param fileName string. Either a name if a default list is used
#'  (currently available is "DDExample") or a full directory to the file that
#'  should be read in.
#' @param ddList data.frame. The data frame specifying the outcome time pairs
#'  needed to create the tables. Is not used when loadFile is set to TRUE.
#' @param sepFile string. The separator in the to be loaded file. Is passed to
#' sep in read.table.
#' @param defaultList logical. If TRUE (default is FALSE) a default list from
#'  the package is be used.
#' @param randomizeVertical logical. If TRUE (default) the order of
#'  the trials (i.e. the rows) is randimized.
#' @param practiceTrial data.frame. Of the same structure as ddList containing one
#'  practice trial. Only has to be specified if withPracticeTrial is set to TRUE
#'  and practiceFileName is NULL.
#' @param withPracticeTrial logical. If TRUE (default is FALSE) a practice
#'  trial has to be provided and is included.
#' @param practiceFileName strin. Either a name if a default list is used
#'  (currently available is "DDExample") or a full directory to the file that
#'  should be read in.
#' @param outcomeCurrency string. The currency that should be
#'  displayed after the outcome values (e.g. "$"). If not specified only the
#'  numbers will be displayed.
#' @importFrom utils read.table
#'
#' @return A list containing the prepared html tables of the outcomes, the data
#'  frame from the input file or list, the trial order and the number of trials.
#' @export
#'
#' @examples
#'   ddContainer <- createDdList(defaultList = TRUE, fileName = "DDExample",
#'                               withPracticeTrial = TRUE, outcomeCurrency = "$")
#'   ddContainer
createDdList <- function(loadFile = TRUE,
                         fileName = "DDExample", ddList = NULL,
                         sepFile = "\t", defaultList = FALSE,
                         randomizeVertical = TRUE,
                         practiceTrial = NULL, withPracticeTrial = FALSE,
                         practiceFileName = NULL, outcomeCurrency = ""){

  # read in files
  if (isTRUE(loadFile)){

    if (isTRUE(defaultList)){
      fil <- system.file("extdata", paste0(fileName, ".txt"),
                         package = "ShinyPsych")
      dd.df <- read.table(fil, header = TRUE, sep = "\t",
                          stringsAsFactors = FALSE)

    } else {
      dd.df <- read.table(fileName, header = TRUE, sep = sepFile,
                              stringsAsFactors = FALSE)
    }
  } else {
    dd.df <- ddList

  }

  # read in practice trial files
  if (isTRUE(withPracticeTrial)){

    if (isTRUE(defaultList)){
      filPr <- system.file("extdata", paste0(fileName, "_Practice.txt"),
                         package = "ShinyPsych")
      practice.df <- read.table(filPr, header = TRUE, sep = "\t",
                                stringsAsFactors = FALSE)

    } else if (!is.null(practiceFileName)){

      practice.df <- read.table(fileName, header = TRUE, sep = sepFile,
                                stringsAsFactors = FALSE)

    } else if (!is.null(practiceTrial)){

      if (is.data.frame(practiceTrial)){

        practice.df <- practiceTrial

      } else {

        stop(paste("\"practiceTrial\" must be of class \"data.frame\", yours is",
                   class(practiceTrial)))

      }

    } else {
      stop("withPracticeTrial is TRUE but no input on where to find the trial info in one of \"defaultGambles\", \"practiceFileName\" or \"practiceTrial\".")
    }
  }

  d.rows <- nrow(dd.df)

  if (isTRUE(randomizeVertical)){
    dd.df <- dd.df[sample(nrow(dd.df)), ]
  }

  # get outcomes
  outcomes <- lapply(seq_len(d.rows), .getDdOutcomes, df = dd.df, nOpt = 2,
                     currency = outcomeCurrency)

  if (isTRUE(withPracticeTrial)){
    outcomes.practice <- lapply(seq_len(nrow(practice.df)), .getDdOutcomes,
                                df = practice.df, nOpt = 2,
                                currency = outcomeCurrency)

    outcomes <- c(outcomes.practice, outcomes)
  }



  if (isTRUE(withPracticeTrial)){
    dd.df <- rbind(practice.df, dd.df)
  }


  container <- list("outcomes" = outcomes,
                    "dd.df" = dd.df,
                    "trial.order" = dd.df$trial,
                    "nTrials" = nrow(dd.df))

  container

}
