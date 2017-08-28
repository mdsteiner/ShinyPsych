#' Create a List of Outcome Values to Use in the Bandit Task
#'
#' Create a list of outcome values from (possibly different) distributions.
#' These values will then be displayed in the bandit task.
#'
#' If a file with the distribution info is specified it has to be either an
#' rds file or an RData file. Else a list must be given to distList.
#'
#' A valid list must have the following options:
#' A vector of number of trials named "nTrials" specifying the number of trials
#' each game takes. If different distribution types should be used a matrix of
#' the distribution type each option in each game takes, else a single string
#' indicating the distribution. The arguments the specific distributions take.
#' The names of these arguments must match the argument names from the distribution
#' documentations. If different distributions are used the number of rows of the
#' matrix must match the number of games, use placeholder for the rows where a
#' specific distribution is not used (see example). If the same distribution type
#' is used it is of course also possible to give a matrix with always the same
#' type but this will be slower because checks are on different levels.
#' @param nArms integer. The number of arms (i.e. options) the bandit task
#'  will have.
#' @param roundDigits integer. The number of digits after the comma outcome
#'  values should be rounded to. This is passed to \code{\link[base]{round}}.
#' @param loadList logical. If TRUE the list specifying the distribution will
#'  be loaded from a local file.
#' @param distList list. Contains information about the distributions from which
#'  the values should be drawn.
#' @param differentDists logical. If TRUE the outcomes of different options can
#'  be drawn from different distribution types. If FALSE it is assumed
#'  that values over all games come from the same distribution type. Valid
#'  inputs are "normal", "exp", "unif", "beta" or "exgauss".
#' @param fileName string. The name of the file with the distribution info. Must
#'  be specified if loadList is set to TRUE.
#' @param randomize logical. If TRUE (default) the option positions are
#'  randomized.
#' @importFrom stats rbeta rexp rnorm runif
#'
#' @return A list containing a list of the outcome values, their positioning
#'  order in option.order and the distList.
#' @export
#'
#' @examples
#' ### example with a single distribution type for a four armed bandit task:
#'
#' # first create the list which as the distribution info
#' banditDistList <- list("nTrials" = c(5, rep(10, 4)),
#'                        "distributionType" = "normal",
#'                        "mean" = matrix(c(4, 4, 4, 4, rep(c(2, 4, 3, 4), 4)),
#'                                 ncol = 4, byrow = TRUE),
#'                        "sd" = matrix(c(3, 3, 3, 3, rep(c(2.5, 11, 5, 7), 4)),
#'                                 ncol = 4, byrow = TRUE))
#'
#' # then call createBanditList
#' banditContainer <- createBanditList(nArms = 4, roundDigits = 1,
#'                                     distList = banditDistList)
#'
#' ### example with a different distribution types for a two armed bandit task:
#'
#' # first create the list which as the distribution info
#' banditDistList <- list("nTrials" = c(5, rep(10, 2)),
#'                        "distributionType" = matrix(c("normal", "normal",
#'                                                      "unif", "exp",
#'                                                      "exgauss", "beta",
#'                                                      "normal", "unif"),
#'                                 ncol = 2),
#'                        "mean" = matrix(c(4, 4, rep(c(2, 4), 4)), # for rnorm
#'                                 ncol = 2, byrow = TRUE),
#'                        "sd" = matrix(c(3, 3, rep(c(2.5, 11), 4)),# for rnorm
#'                                 ncol = 2, byrow = TRUE),
#'                        "min" = matrix(rep(1, 8),                 # for runif
#'                                 ncol = 2, byrow = TRUE),
#'                        "max" = matrix(rep(10, 8),                # for runif
#'                                 ncol = 2, byrow = TRUE),
#'                        "rate" = matrix(rep(1, 8),                # for rexp
#'                                 ncol = 2, byrow = TRUE),
#'                        "mu" = matrix(rep(4, 8),                  # for rexgauss
#'                                 ncol = 2, byrow = TRUE),
#'                        "sigma" = matrix(rep(2.5, 8),             # for rexgauss
#'                                 ncol = 2, byrow = TRUE),
#'                        "tau" = matrix(rep(2, 8),                 # for rexgauss
#'                                 ncol = 2, byrow = TRUE),
#'                        "positive" = matrix(rep(FALSE, 8),        # for rexgauss
#'                                 ncol = 2, byrow = TRUE),
#'                        "shape1" = matrix(rep(2, 8),              # for rbeta
#'                                 ncol = 2, byrow = TRUE),
#'                        "shape2" = matrix(rep(3, 8),              # for rbeta
#'                                 ncol = 2, byrow = TRUE),
#'                        "ncp" = matrix(rep(0.5, 8),               # for ncp
#'                                 ncol = 2, byrow = TRUE))
#'
#' # then call createBanditList
#' banditContainer <- createBanditList(nArms = 2, roundDigits = 2,
#'                                     distList = banditDistList,
#'                                     differentDists = TRUE)
createBanditList <- function(nArms = 2, roundDigits = 0, loadList = FALSE,
                             distList = NULL, differentDists = FALSE,
                             fileName = NULL, randomize = TRUE){

  if (loadList == "complete list"){
    return(load(fileName))

  } else if (loadList == "dist info"){

    if (length(grep(".rds", fileName)) > 0){

      distList <- readRDS(fileName)

    } else if (length(grep(".RData", fileName)) > 0){

      load(fileName)

    }
  } else if (!is.null(fileName)){
    stop(paste(loadList, "is no valid loadList input. Must be either \"complete list\", \"dist info\" or \"NULL\" and if it is not NULL fileName must be specified."))
  }


  if (isTRUE(differentDists)){

    outcomes <- lapply(seq_along(distList$nTrials), .getDiffDistVals1,
                       distributionList = distList, Arms = nArms,
                       nDigits = roundDigits)

  } else {

    outcomes <- .getSameDistVals(dList = distList, armNumber = nArms,
                                rDigits = roundDigits)

  }

  if (isTRUE(randomize)){
    locations.r <- matrix(NA, ncol = nArms, nrow = length(distList$nTrials))
    # Randomize option locations
    for (ga in seq_along(distList$nTrials)){
      locations.r[ga, 1:nArms] <- sample(1:nArms)
      outcomes[[ga]] <- outcomes[[ga]][,locations.r[ga,]]
    }

    option.order <- vector("list", length(distList$nTrials))
    for (rows in 1:nrow(locations.r)){
      temp.order <- paste(locations.r[rows,], collapse = ";")
      option.order[[rows]] <- rep(temp.order, distList$nTrials[rows])
    }

    option.order <- unlist(option.order)

  } else {

    option.order <- rep(paste(seq_len(nArms), collapse = ";"),
                        sum(distList$nTrials * length(distList$nTrials)))

  }
    container <- list("option.order" = option.order,
                      "distList" = distList,
                      "outcomes" = outcomes)

    container
}
