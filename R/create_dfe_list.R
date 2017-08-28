#' Create a List of Prepared Values to Display Later in a Decisions from Experience
#' Task
#'
#' Create a list of values drawn from the specified gambles that are then later
#' displayed. The values, i.e. outcome and probabilities etc. must be specified
#' and given as input. For each gamble a vector of length nDraws is created
#' containing values of the outcomes drawn from \code{\link[base]{sample}} with the
#' in the list specified probabilities. These values can later be sampled by
#' calling \code{\link{multiOptsDfePage}} that will set up the html logic.
#'
#' \bold{Currently available default lists:}
#' "TwoOptExample", "ThreeOptExample","FourOptExample", "Birnbaum" mostly from
#' Birnbaum (2008), "BrooksPetersZank" from Brooks, Peters and Zank (2014),
#' "GloecknerPachur" a subset from Gloeckner and Pachur (2012),
#' "HertwigBarronWeberErev" from Hertwig, Barron, Weber and Erev (2004),
#' "LoomesMoffattSugden" from Loomes, Moffatt ans Sugden (2002) and "Rieskamp",
#' from Rieskamp (2008).
#' @references Birnbaum, M. H. (2008). New paradoxes of risky decision making.
#'  \emph{Psychological review}, 115(2), 463.
#' @references Brooks, P., Peters, S., & Zank, H. (2014). Risk behavior for gain,
#'  loss, and mixed prospects. \emph{Theory and decision}, 77(2), 153-182.
#' @references Gloeckner, A., & Pachur, T. (2012). Cognitive models of risky
#'  choice: Parameter stability and predictive accuracy of prospect theory.
#'  \emph{Cognition}, 123(1), 21-32.
#' @references Hertwig, R., Barron, G., Weber, E. U., & Erev, I. (2004).
#'  Decisions from experience and the effect of rare events in risky choice.
#'  \emph{Psychological science}, 15(8), 534-539.
#' @references Loomes, G., Moffatt, P. G., & Sugden, R. (2002). A
#'  microeconometric test of alternative stochastic theories of risky choice.
#'  \emph{Journal of risk and Uncertainty}, 24(2), 103-130.
#' @references Rieskamp, J. (2008). The probabilistic nature of preferential
#'  choice. \emph{Journal of Experimental Psychology: Learning, Memory, and
#'  Cognition}, 34(6), 1446.
#' @param nOpts integer. The number of gambles to be presented in each trial.
#'  Currently all trials need to have the same number of options. If you have
#'  different number of options create two separate lists
#' @param nOutcs integer. The maximum number of outcomes a gamble can have, i.e.
#'  if the list contains one 5 outcome gamble and the rest 2 outcome gambles
#'  then set this value to 5. Different number of outcomes in a gamble list are
#'  possible.
#' @param loadFile logical. If TRUE (default) the list specifying
#'  the gambles will be loaded from a local file.
#' @param fileName string. Either a name if a default list is used
#'  or a full directory to the file that should be read in. See details for
#'  currently available default lists.
#' @param gambleList data.frame. Values specifying the gamble parameters. Only
#' needed if loadFile is FALSE.
#' @param sepFile string. The separator in the to be loaded file. Is
#'  passed to sep in read.table.
#' @param defaultGambles logical. If TRUE (default is FALSE) one of the
#'  default lists is used.
#' @param nDraws integer. Specifies the lenght of the vector containing the
#'  gamble outcomes. This is the maximum number of samples the participant can
#'  take. Set this to a large enough value from which you expect that it will
#'  not be reached (default is 100).
#' @param randomizeHorizontal logical. If TRUE (default) options
#'  displayed in one trial are positioned in random order.
#' @param randomizeVertical logical. If TRUE (default) trial order is randomized
#' @param practiceTrial data.frame. Contains information for a practice trial.
#'  Is only needed if withPracticeTrial is TRUE and practiceFileName is NULL.
#' @param withPracticeTrial logical. If TRUE (default is FALSE) a practice
#'  trial has to be provided and is included.
#' @param practiceFileName string. Full path and file name of the
#'  file to load the practice trial info in
#' @importFrom utils read.table
#'
#' @return A list containing lists with the option order (horizontal order),
#'  outcomes, data frame containing the gamble parameters, number of draws from
#'  a gamble, number of options, number of outcomes and gamble order (vertical
#'  order).
#' @export
#' @examples
#' dfeContainer <- createDfeList(defaultGambles = TRUE,
#'                               fileName = "HertwigBarronWeberErev",
#'                               withPracticeTrial = TRUE)
#' dfeContainer
createDfeList <- function(nOpts = NULL, nOutcs = NULL, loadFile = TRUE,
                          fileName = "TwoOptExample", gambleList = NULL,
                          sepFile = "\t", defaultGambles = FALSE, nDraws = 100,
                          randomizeHorizontal = TRUE, randomizeVertical = TRUE,
                          practiceTrial = NULL, withPracticeTrial = FALSE,
                          practiceFileName = NULL){

  # read in files
  if (isTRUE(loadFile)){

    if (isTRUE(defaultGambles)){
      fil <- system.file("extdata", paste0(fileName, ".txt"),
                         package = "ShinyPsych")
      gamble.df <- read.table(fil, header = TRUE,
                              sep = "\t", stringsAsFactors = FALSE)

      default.list.names <- c("HertwigBarronWeberErev", "Birnbaum",
                              "GloecknerPachur", "BrooksPetersZank", "Rieskamp",
                              "LoomesMoffattSugden", "TwoOptExample",
                              "ThreeOptExample", "FourOptExample",
                              "FiveOptExample", "SixOptExample")
      default.list.opts <- c(rep(2, 7), 3:6)
      default.list.outc <- c(2, 5, 5, 5, 2, 3, rep(2, 5))

      nOpts <- default.list.opts[default.list.names == fileName]
      nOutcs <- default.list.outc[default.list.names == fileName]

    } else {
      gamble.df <- read.table(fileName, header = TRUE, sep = sepFile,
                              stringsAsFactors = FALSE)
    }
  } else {
    gamble.df <- gambleList

  }

  # read in practice trial files
  if (isTRUE(withPracticeTrial)){

    if (isTRUE(defaultGambles)){
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

  # prepare variables
  opts.vec <- paste0("g", 1:nOpts)
  outc.vec <- paste0("o", 1:nOutcs)
  prob.vec <- paste0("p", 1:nOutcs)

  ind.outc.vec <- do.call(paste, c(expand.grid(opts.vec, outc.vec), sep = ""))
  ind.prob.vec <- do.call(paste, c(expand.grid(opts.vec, prob.vec), sep = ""))

  g.rows <- nrow(gamble.df)

  if (isTRUE(randomizeVertical)){
    gamble.df <- gamble.df[sample(nrow(gamble.df)), ]
  }

  # get outcomes
  outcomes <- lapply(seq_len(g.rows), .getDfeSamples, p.ind = ind.prob.vec,
                     o.ind = ind.outc.vec, df = gamble.df, nOpt = nOpts,
                     nVals = nDraws)

  if (isTRUE(withPracticeTrial)){
    outcomes.practice <- lapply(seq_len(nrow(practice.df)), .getDfeSamples,
                                p.ind = ind.prob.vec, o.ind = ind.outc.vec,
                                df = practice.df, nOpt = nOpts, nVals = nDraws)

    outcomes <- c(outcomes.practice, outcomes)
  }



  if (isTRUE(withPracticeTrial)){
    gamble.df <- rbind(practice.df, gamble.df)
    g.rows <- nrow(gamble.df)
  }

  if (isTRUE(randomizeHorizontal)){
    locations.r <- matrix(NA, ncol = nOpts, nrow = g.rows)
    # Randomize option locations
    for (ro in seq_len(g.rows)){
      locations.r[ro, seq_len(nOpts)] <- sample(seq_len(nOpts))
      outcomes[[ro]] <- outcomes[[ro]][locations.r[ro,]]
    }

    option.order <- vector("list", g.rows)
    for (rows in seq_len(nrow(locations.r))){
      temp.order <- paste(locations.r[rows,], collapse = ";")
      option.order[[rows]] <- rep(temp.order, nDraws)
    }

    option.order <- unlist(option.order)

  } else {

    option.order <- rep(paste(seq_len(nOpts), collapse = ";"), nDraws * g.rows)
  }

  container <- list("option.order" = option.order,
                    "outcomes" = outcomes,
                    "gamble.df" = gamble.df,
                    "nDraws" = nDraws,
                    "nOpts" = nOpts,
                    "nOutcs" = nOutcs,
                    "gamble.order" = gamble.df$gamble)

  container

}
